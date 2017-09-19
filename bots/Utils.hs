{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Network.TLS
import System.Clock
import System.Posix.Env.ByteString
import System.Remote.Monitoring
import System.IO

import Hircine


userAgent :: ByteString
userAgent = "Hircine (https://git.io/hircine-issues)"


startBot
    :: ConnectionParams
    -> (ByteString -> Manager -> Hircine Acceptor) -> IO ()
startBot params makeBot = do
    hSetBuffering stdout LineBuffering
    channel <- getEnv' "HIRCINE_CHANNEL"
    nick <- getEnv' "HIRCINE_NICK"
    secret <- getEnv' "HIRCINE_SECRET"
    ekgPort <- fmap (read . BC.unpack) <$> getEnv "HIRCINE_EKG_PORT"
    for_ ekgPort $ \p -> do
        _ <- forkServer "localhost" p
        putStrLn $ "started EKG server on port " ++ show p
    man <- newManager tlsManagerSettings
    context <- initConnectionContext
    putStrLn "connecting..."
    mainLoop channel nick secret man context `finally` putStrLn "au revoir"
  where
    mainLoop channel nick secret man context = logExceptionsAndRetry $
        connect context params $ \conn -> do
            putStrLn $ "connected to " ++ show (connectionID conn)
            sendLock <- newMVar ()
            let stream = makeStream
                    (connectionGetLine 1024 conn)
                    (\x -> withMVar sendLock $ \_ -> connectionPut conn x)
            runHircine (logMessages $ start channel nick secret man) stream

    start channel nick secret man = do
        when (not $ BC.null secret) $ send $ Pass secret
        send $ Nick nick
        send $ User nick "report bugs to https://git.io/hircine-issues"
        waitForWelcome
        send $ Join [channel] Nothing
        bot <- makeBot channel man
        let respond = runAcceptor $ replyToPing >> bot
        forever $ receive >>= respond


connect :: ConnectionContext -> ConnectionParams -> (Connection -> IO r) -> IO r
connect context params = bracket (connectTo context params) connectionClose


logMessages :: Hircine r -> Hircine r
logMessages = local $ \s -> s
    { streamReceive = do
        m <- streamReceive s
        putStrLn $ "<- " ++ showMessage m
        return m
    , streamSend = \cs -> do
        for_ cs $ \c -> putStrLn $ "-> " ++ showCommand c
        streamSend s cs
    }


waitForWelcome :: Hircine ()
waitForWelcome = do
    message@(Message _ (Command code _)) <- receive
    when (code /= "001") $ do
        runAcceptor replyToPing message
        waitForWelcome


logExceptionsAndRetry :: forall a. IO a -> IO a
logExceptionsAndRetry action = do
    now <- getMonotonicTime
    start now 0
  where
    start :: TimeSpec -> Int -> IO a
    start lastRetryTime failCount = do
        x <- try $ try action
        case x of
            Right (Right r) -> return r
            Right (Left e) -> retry lastRetryTime failCount (e :: IOException)
            Left e -> retry lastRetryTime failCount (e :: TLSException)

    retry :: Exception e => TimeSpec -> Int -> e -> IO a
    retry lastRetryTime failCount e = do
        printError $ show e
        now <- getMonotonicTime
        if now - lastRetryTime < successThreshold
            then if failCount >= maxTries
                then throwIO e
                else do
                    let delaySecs = 2 ^ failCount
                    putStrLn $ "reconnecting in " ++ show delaySecs ++ " seconds"
                    threadDelay $ delaySecs * 1000 * 1000
                    start now (failCount + 1)
            else do
                putStrLn $ "reconnecting"
                start lastRetryTime 0

-- | If a bot stays up for this long, it is considered successful.
successThreshold :: TimeSpec
successThreshold = TimeSpec { sec = 15 * 60, nsec = 0 }

maxTries :: Int
maxTries = 5


makeHttpRequest :: String -> Manager -> IO (Maybe BL.ByteString)
makeHttpRequest url man = do
    r <- try $ httpLbs req man
    case r of
        Left e ->
            printError' (e :: SomeException) >> return Nothing
        Right r' | not . statusIsSuccessful $ responseStatus r' ->
            printError' (responseStatus r') >> return Nothing
        Right r' -> return $ Just (responseBody r')
  where
    req = (parseRequest_ url) { requestHeaders = [("User-Agent", userAgent)] }


getEnv' :: ByteString -> IO ByteString
getEnv' name = getEnv name
    >>= maybe (error $ "missing environment variable " ++ show name) return


printError :: String -> IO ()
printError = putStrLn . ("** ERROR: " ++)

printError' :: Show a => a -> IO ()
printError' = printError . show


type Acceptor = ReaderT Message Hircine ()

runAcceptor :: Acceptor -> Message -> Hircine ()
runAcceptor = runReaderT

nullAcceptor :: Acceptor
nullAcceptor = return ()

replyToPing :: Acceptor
replyToPing = accept $ \(Ping a b) -> send $ Pong a b

accept :: IsCommand a => (a -> Hircine ()) -> Acceptor
accept = accept' . const

accept' :: IsCommand a => (Maybe Origin -> a -> Hircine ()) -> Acceptor
accept' k = ReaderT $ \(Message origin command) ->
    mapM_ (k origin) $ fromCommand command


data Crate = Crate
    { crateName :: !Text
    , crateVersion :: !Text
    , crateDescription :: !Text
    } deriving (Eq, Show)


type CrateMap = Map Text (Text, Text)

defaultCrateMap :: CrateMap
defaultCrateMap = Map.empty

fetchAndUpdateCrateMap
    :: String
        -- ^ URL to package feed
    -> (BL.ByteString -> Maybe [Crate])
        -- ^ Function to parse crate data
    -> IORef CrateMap
    -> Manager
    -> IO [Crate]
fetchAndUpdateCrateMap feedUrl parseCrates crateMap man = do
    maybeBytes <- makeHttpRequest feedUrl man
    case maybeBytes of
        Just bytes -> case parseCrates bytes of
            Just crates -> atomicModifyIORef' crateMap $ updateCrateMap crates
            Nothing -> do
                printError "could not parse packages!!!"
                print bytes
                return []
        Nothing -> return []

-- | Update the internal crate map. Return the set of crates that were changed.
-- The list of changed crates is sorted in ascending order by name.
updateCrateMap :: [Crate] -> CrateMap -> (CrateMap, [Crate])
updateCrateMap newCrates' oldCrates = (newCrates, changedCrates)
  where
    newCrates = toCrateMap newCrates'
    changedCrates
        | Map.null oldCrates = []
        | otherwise = fromCrateMap $ Map.differenceWith
            (\new old -> if new /= old then Just new else Nothing)
            newCrates oldCrates

toCrateMap :: [Crate] -> CrateMap
toCrateMap crates = Map.fromList
    [ (crateName p, (crateVersion p, crateDescription p))
    -- Map.fromList takes the last value in case of duplicates, but in a
    -- chronological feed we usually want the first value instead. Reversing
    -- the input list should mitigate this issue.
    | p <- reverse crates ]

fromCrateMap :: CrateMap -> [Crate]
fromCrateMap crates = [ Crate name vers desc |
    (name, (vers, desc)) <- Map.toList crates ]


showCrate :: Text -> Crate -> Text
showCrate baseUrl p = Text.intercalate " – "
    [ showCrateNameVersion p
    , clipTo 80 . collapseSpaces $ crateDescription p
    , baseUrl <> crateName p
    ]
  where
    clipTo :: Int -> Text -> Text
    clipTo n s
        | Text.length s <= n = s
        | otherwise =
            (Text.dropWhileEnd isSpace . Text.dropWhileEnd (not . isSpace) $
                Text.take (n + 1) s)
            <> "…"

    collapseSpaces :: Text -> Text
    collapseSpaces = Text.unwords . Text.words

showCrateNameVersion :: Crate -> Text
showCrateNameVersion p = crateName p <> " " <> crateVersion p


getMonotonicTime :: IO TimeSpec
getMonotonicTime = getTime Monotonic

toMicroSecs :: TimeSpec -> Int
toMicroSecs = ceiling . (/ (1000 :: Rational)) . fromInteger . toNanoSecs

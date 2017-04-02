{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Utils where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import System.Posix.Env.ByteString
import System.Remote.Monitoring
import System.IO

import Hircine


userAgent :: ByteString
userAgent = "Brigitte (https://git.io/brigitte)"


startBot
    :: ConnectionParams
    -> ByteString  -- ^ Nick
    -> (forall r. ByteString -> Manager -> Hircine r) -> IO ()
startBot params nick bot = do
    hSetBuffering stdout LineBuffering
    channel <- getEnv' "BRIGITTE_CHANNEL"
    secret <- getEnv' "BRIGITTE_SECRET"
    ekgPort <- fmap (read . BC.unpack) <$> getEnv "BRIGITTE_EKG_PORT"
    for_ ekgPort $ \p -> do
        _ <- forkServer "localhost" p
        putStrLn $ "Started EKG server on port " ++ show p
    man <- newManager tlsManagerSettings
    context <- initConnectionContext
    putStrLn "Connecting..."
    connect context params $ \conn -> do
        putStrLn $ "Connected to " ++ show (connectionID conn)
        let stream = makeStream
                (connectionGetLine 1024 conn)
                (connectionPut conn)
        runHircine (logMessages $ start channel secret man) stream
            `finally` putStrLn "Au revoir"
  where
    start channel secret man = do
        when (not $ BC.null secret) $ send $ Pass secret
        send $ Nick nick
        send $ User nick "Report issues to https://git.io/brigitte"
        bot channel man


connect :: ConnectionContext -> ConnectionParams -> (Connection -> IO r) -> IO r
connect context params = bracket (connectTo context params) connectionClose


logMessages :: Hircine r -> Hircine r
logMessages = local $ \s -> s
    { hsReceive = do
        m <- hsReceive s
        putStrLn $ showMessage m
        return m
    , hsSend = \cs -> do
        for_ cs $ \c -> putStrLn $ " -> " ++ showCommand c
        hsSend s cs
    }


makeHttpRequest :: String -> Manager -> IO (Maybe BL.ByteString)
makeHttpRequest url man = do
    r <- try $ httpLbs req man
    case r of
        Left e ->
            printError (e :: SomeException) >> return Nothing
        Right r' | not . statusIsSuccessful $ responseStatus r' ->
            printError (responseStatus r') >> return Nothing
        Right r' -> return $ Just (responseBody r')
  where
    req = (parseRequest_ url) { requestHeaders = [("User-Agent", userAgent)] }

    printError :: Show a => a -> IO ()
    printError e = putStrLn $ "** ERROR: " ++ show e


getEnv' :: ByteString -> IO ByteString
getEnv' name = getEnv name
    >>= maybe (error $ "missing environment variable " ++ show name) return


-- FIXME this looks icky
handlePing :: Command -> Hircine ()
handlePing = makeHandler $ \(Ping a b) -> send $ Pong a b

handleQuit :: (ByteString -> Bool) -> Maybe Origin -> Command -> Hircine ()
handleQuit isAdmin origin = makeHandler $ \(PrivMsg _ msg) ->
    case origin of
        Just (FromUser nick _ _)
            | isAdmin nick && msg `elem` ["quit", "stop"] ->
                send $ Quit (Just $ "kicked by " <> nick)
        _ -> return ()

makeHandler :: (Monad m, IsCommand a) => (a -> m ()) -> Command -> m ()
makeHandler k = mapM_ k . fromCommand


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
                putStrLn $ "** ERROR: could not parse packages!!!"
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
    | p <- crates ]

fromCrateMap :: CrateMap -> [Crate]
fromCrateMap crates = [ Crate name vers desc |
    (name, (vers, desc)) <- Map.toList crates ]

{-# LANGUAGE OverloadedStrings #-}


import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Default.Class
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Monoid
import qualified Data.Text.Encoding as Text
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Posix.Env.ByteString
import System.Remote.Monitoring
import System.IO

import Hircine

import Types


userAgent :: ByteString
userAgent = "Brigitte (https://git.io/brigitte)"


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    channel <- getEnv' "BRIGITTE_CHANNEL"
    secret <- getEnv' "BRIGITTE_SECRET"
    ekgPort <- fmap (read . BC.unpack) <$> getEnv "BRIGITTE_EKG_PORT"
    for_ ekgPort $ \p -> do
        _ <- forkServer "localhost" p
        putStrLn $ "Started EKG server on port " ++ show p
    man <- newManager tlsManagerSettings
    crateMap <- newIORef defaultCrateMap
    context <- initConnectionContext
    let params = ConnectionParams {
        connectionHostname = "irc.mozilla.org",
        connectionPort = 6697,
        connectionUseSecure = Just def,
        connectionUseSocks = Nothing
        }
    connect context params $ \conn -> do
        putStrLn $ "Connected to " ++ show (connectionID conn)
        let stream = makeStream
                (connectionGetLine 1024 conn)
                (connectionPut conn)
        runHircine (logMessages $ bot channel secret crateMap man) stream
            `finally` putStrLn "Closing"


bot :: ByteString -> ByteString -> IORef CrateMap -> Manager -> Hircine ()
bot channel secret crateMap man = do
    send $ Pass secret
    send $ Nick "brigitte"
    send $ User "brigitte" "SCP-191 is a good IRC bot"
    forever $ do
        Message origin command <- receive
        return command
            ? (\(Ping a b) -> send $ Pong a b)
            ? (\(PrivMsg _ msg) ->
                case origin of
                    Just (FromUser nick _ _)
                        | nick `elem` ["lfairy", "nrc"]
                                && msg `elem` ["quit", "stop"] ->
                            send $ Quit (Just $ "kicked by " <> nick)
                    _ -> return () )
            ? (\(Command code _) ->
                when (code == "900") $ do
                    send $ Join [channel] []
                    _ <- fork $ checkNewCrates channel crateMap man
                    return () )


checkNewCrates :: ByteString -> IORef CrateMap -> Manager -> Hircine ()
checkNewCrates channel crateMap man = forever $ do
    changedCrates <- liftIO $ do
        initReq <- parseUrl "https://crates.io/summary"
        let req = initReq {
            requestHeaders = ("User-Agent", userAgent) : requestHeaders initReq
            }
        r <- try $ httpLbs req man
        case r of
            Left e -> do
                putStrLn $ "ERROR: " ++ show (e :: HttpException)
                return []
            Right r' ->
                let maybeJson = decode $ responseBody r'
                    crates = extractCrates =<< toList maybeJson
                in  atomicModifyIORef' crateMap $ updateCrateMap crates
    buffer . for_ changedCrates $
        send . PrivMsg [channel] . Text.encodeUtf8 . showCrate
    liftIO . threadDelay $ 60 * 1000 * 1000  -- 60 seconds
  where
    extractCrates :: Value -> [Crate]
    extractCrates (Object v) = [ crate |
        Array entries <- toList $ HashMap.lookup "just_updated" v,
        Success crate <- map fromJSON $ toList entries ]
    extractCrates _ = []


logMessages :: Hircine () -> Hircine ()
logMessages = local $ \s -> s {
    hsReceive = do
        m <- hsReceive s
        putStrLn $ showMessage m
        return m,
    hsSend = \cs -> do
        for_ cs $ \c -> putStrLn $ " -> " ++ showCommand c
        hsSend s cs
    }


connect :: ConnectionContext -> ConnectionParams -> (Connection -> IO r) -> IO r
connect context params = bracket (connectTo context params) connectionClose


getEnv' :: ByteString -> IO ByteString
getEnv' name = getEnv name
    >>= maybe (error $ "missing environment variable " ++ show name) return

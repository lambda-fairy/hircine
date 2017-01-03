{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Utils where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
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
        send $ User nick "Bot managed by lfairy. Report issues to https://git.io/brigitte"
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


makeHttpRequest :: String -> Manager -> Hircine (Maybe BL.ByteString)
makeHttpRequest url man = liftIO $ do
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

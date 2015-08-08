{-# LANGUAGE OverloadedStrings #-}


import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Acid
import Data.Acid.Local
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Typeable
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Simple.TCP (connect)
import System.Posix.Env.ByteString
import System.Remote.Monitoring
import System.IO

import Hircine

import Types


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
    withAcidState defaultBrigitteState $ \acid ->
        connect "irc.mozilla.org" "6667" $ \(sock, addr) -> do
            putStrLn $ "Connected to " ++ show addr
            (is, os) <- socketToIrcStreams sock
            runHircine (logMessages $ bot channel secret acid man) is os
                `finally` putStrLn "Closing"


bot :: ByteString -> ByteString
    -> AcidState BrigitteState -> Manager -> Hircine ()
bot channel secret acid man = do
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
                    _ <- fork $ checkNewCrates channel acid man
                    return () )


checkNewCrates :: ByteString -> AcidState BrigitteState -> Manager -> Hircine ()
checkNewCrates channel acid man = forever $ do
    changedCrates <- liftIO $ do
        req <- parseUrl "https://crates.io/summary"
        r <- try $ httpLbs req man
        case r of
            Left e -> do
                putStrLn $ "ERROR: " ++ show (e :: HttpException)
                return []
            Right r' ->
                let maybeJson = decode $ responseBody r'
                    crates = extractCrates =<< toList maybeJson
                in  update acid $ UpdateCrates crates
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


getEnv' :: ByteString -> IO ByteString
getEnv' name = getEnv name
    >>= maybe (error $ "missing environment variable " ++ show name) return


withAcidState
    :: (Typeable st, IsAcidic st) => st -> (AcidState st -> IO a) -> IO a
withAcidState initialState
    = bracket (openLocalState initialState) createCheckpointAndClose

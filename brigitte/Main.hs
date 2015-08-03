{-# LANGUAGE OverloadedStrings #-}


import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Acid
import Data.Acid.Local
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Typeable
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (HttpException)
import Network.Simple.TCP (connect)
import Network.Wreq
import qualified Network.Wreq.Session as S
import System.Posix.Env.ByteString
import System.IO

import Hircine

import Types


channel :: ByteString
channel = "#brigitte"


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    secret <- getEnv' "BRIGITTE_SECRET"
    S.withSession $ \sess ->
        withAcidState defaultBrigitteState $ \acid ->
            connect "irc.mozilla.org" "6667" $ \(sock, addr) -> do
                putStrLn $ "Connected to " ++ show addr
                (is, os) <- socketToIrcStreams sock
                runHircine (logMessages $ bot secret acid sess) is os
                    `finally` putStrLn "Closing"


bot :: ByteString -> AcidState BrigitteState -> S.Session -> Hircine ()
bot secret acid sess = do
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
                    _ <- fork $ checkNewCrates acid sess
                    return () )


checkNewCrates :: AcidState BrigitteState -> S.Session -> Hircine ()
checkNewCrates acid sess = forever $ do
    changedCrates <- liftIO $ do
        r <- try $ S.get sess "https://crates.io/summary"
        case r of
            Left e -> do
                putStrLn $ "ERROR: " ++ show (e :: HttpException)
                return []
            Right r' ->
                let crates = mapMaybe fromJSON' $
                        r' ^.. responseBody . key "just_updated" . values
                in  update acid $ UpdateCrates crates
    buffer . for_ changedCrates $
        send . PrivMsg [channel] . Text.encodeUtf8 . showCrate
    liftIO . threadDelay $ 60 * 1000 * 1000  -- 60 seconds


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


fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' v = case fromJSON v of
    Error _ -> Nothing
    Success x -> Just x


withAcidState
    :: (Typeable st, IsAcidic st) => st -> (AcidState st -> IO a) -> IO a
withAcidState initialState
    = bracket (openLocalState initialState) createCheckpointAndClose

{-# LANGUAGE OverloadedStrings #-}


import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Simple.TCP (connect)
import Network.Wreq
import qualified Network.Wreq.Session as S
import System.Posix.Env.ByteString
import System.IO

import Hircine


channel :: ByteString
channel = "#brigitte"


main :: IO ()
main = S.withSession $ \sess -> do
    hSetBuffering stdout LineBuffering
    secret <- getEnv' "BRIGITTE_SECRET"
    connect "irc.mozilla.org" "6667" $ \(sock, addr) -> do
        putStrLn $ "Connected to " ++ show addr
        (is, os) <- socketToIRCStreams sock
        runHircine (logMessages $ bot secret sess) is os
            `finally` putStrLn "Closing"


bot :: ByteString -> S.Session -> Hircine ()
bot secret sess = do
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
                    _ <- fork $ checkNewCrates sess
                    return () )


checkNewCrates :: S.Session -> Hircine ()
checkNewCrates sess = forever $ do
    r <- liftIO $ S.get sess "https://crates.io/summary"
    let packages = mapMaybe fromJSON' $
            (r ^.. responseBody . key "just_updated" . values)
            ++ (r ^.. responseBody . key "new_crates" . values)
    buffer . for_ packages $
        send . PrivMsg [channel] . Text.encodeUtf8 . showPackage
    liftIO . threadDelay $ 10 * 1000 * 1000  -- 10 seconds


data Package = Package {
    packageName :: Text,
    packageMaxVersion :: Text,
    packageDescription :: Text
    } deriving (Eq, Show)

instance FromJSON Package where
    parseJSON (Object v) = Package
        <$> v .: "name"
        <*> v .: "max_version"
        <*> v .: "description"
    parseJSON _ = empty

showPackage :: Package -> Text
showPackage p = Text.intercalate " – " [
    packageName p <> " " <> packageMaxVersion p,
    summarize 80 (packageDescription p),
    "https://crates.io/crates/" <> packageName p
    ]


summarize :: Int -> Text -> Text
summarize limit = Text.unwords . unfoldr f . (,) 0 . Text.words
  where
    f (_, []) = Nothing
    f (n, (w : ws))
        | n' > limit = Just ("…", (n', []))
        | otherwise = Just (w, (n', ws))
      where
        n' = n + Text.length w


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

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char
import Data.Default.Class
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Connection
import Network.HTTP.Client

import Hircine

import Utils


main :: IO ()
main = do
    crateMap <- newIORef defaultCrateMap
    startBot params "[cargobot]" $ \channel man -> forever $ do
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
                    send $ Join [channel] Nothing
                    _ <- fork $ checkNewCrates channel crateMap man
                    return () )
  where
    params = ConnectionParams
        { connectionHostname = "irc.mozilla.org"
        , connectionPort = 6697
        , connectionUseSecure = Just def
        , connectionUseSocks = Nothing
        }


checkNewCrates :: ByteString -> IORef CrateMap -> Manager -> Hircine ()
checkNewCrates channel crateMap man = forever $ do
    changedCrates <- liftIO $ do
        maybeBytes <- makeHttpRequest "https://crates.io/summary" man
        case maybeBytes of
            Nothing -> return []
            Just bytes ->
                let crates = extractCrates =<< toList (decode bytes)
                in  atomicModifyIORef' crateMap $ updateCrateMap crates
    buffer . for_ changedCrates $
        send . PrivMsg [channel] . Text.encodeUtf8 . showCrate
    liftIO . threadDelay $ 60 * 1000 * 1000  -- 60 seconds
  where
    extractCrates :: Value -> [Crate]
    extractCrates (Object v) = [ crate |
        Array justUpdated <- toList $ HashMap.lookup "just_updated" v,
        Array newCrates <- toList $ HashMap.lookup "new_crates" v,
        Success crate <- map fromJSON $ toList justUpdated ++ toList newCrates ]
    extractCrates _ = []


instance FromJSON Crate where
    parseJSON (Object v) = Crate
        <$> v .: "name"
        <*> v .: "max_version"
        <*> v .: "description"
    parseJSON _ = empty

showCrate :: Crate -> Text
showCrate p = Text.intercalate " – "
    [ crateName p <> " " <> crateVersion p
    , clipTo 80 . collapseSpaces $ crateDescription p
    , "https://crates.io/crates/" <> crateName p
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

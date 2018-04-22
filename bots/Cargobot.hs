{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.Text.Encoding as Text
import Network.Connection
import Network.HTTP.Client

import Hircine

import Utils


main :: IO ()
main = do
    crateMap <- newIORef defaultCrateMap
    startBot params $ \channel man -> do
        fork $ checkNewCrates channel crateMap man
        return nullAcceptor
  where
    params = ConnectionParams
        { connectionHostname = "irc.mozilla.org"
        , connectionPort = 6697
        , connectionUseSecure = Just def
        , connectionUseSocks = Nothing
        }


checkNewCrates :: ByteString -> IORef CrateMap -> Manager -> Hircine ()
checkNewCrates channel crateMap man = forever $ do
    changedCrates <- liftIO $ fetchAndUpdateCrateMap feedUrl parseCrates crateMap man
    buffer . for_ changedCrates $
        send . PrivMsg [channel] . Text.encodeUtf8 . showCrate baseUrl
    liftIO . threadDelay $ 5 * 60 * 1000 * 1000  -- 5 minutes
  where
    feedUrl = "https://crates.io/api/v1/summary"
    baseUrl = "https://crates.io/crates/"

    parseCrates :: BL.ByteString -> Maybe [Crate]
    parseCrates bytes
        | Just (Object v) <- decode bytes
        , Just (Array justUpdated) <- HashMap.lookup "just_updated" v
        , Just (Array newCrates) <- HashMap.lookup "new_crates" v
        , Success crates <- traverse fromJSON $ toList justUpdated ++ toList newCrates
        = Just crates
    parseCrates _ = Nothing


instance FromJSON Crate where
    parseJSON (Object v) = Crate
        <$> v .: "name"
        <*> v .: "max_version"
        <*> v .: "description"
    parseJSON _ = empty

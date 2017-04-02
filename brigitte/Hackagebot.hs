{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.IORef
import qualified Data.Text as Text
import Network.Connection
import Network.HTTP.Client
import Text.RSS.Import
import Text.RSS.Syntax
import Text.XML.Light.Input

import Hircine

import Utils


main :: IO ()
main = do
    crateMap <- newIORef defaultCrateMap
    startBot params myNick $ \channel man -> forever $ do
        Message origin command <- receive
        return command
            ? handlePing
            ? handleQuit (== "lfairy") origin
            ? (\(Mode nick _) ->
                when (nick == myNick) $ do
                    send $ Join [channel] Nothing
                    _ <- fork $ checkNewCrates channel crateMap man
                    send $ PrivMsg [channel] "\x01\&ACTION Hello, world!\x01"
                    return () )
  where
    myNick = "hackagebot"
    params = ConnectionParams
        { connectionHostname = "chat.freenode.net"
        , connectionPort = 6697
        , connectionUseSecure = Just def
        , connectionUseSocks = Nothing
        }


checkNewCrates :: ByteString -> IORef CrateMap -> Manager -> Hircine ()
checkNewCrates _channel crateMap man = forever $ do
    changedCrates <- liftIO $ fetchAndUpdateCrateMap feedUrl parseCrates crateMap man
    -- TODO
    liftIO $ print changedCrates
    liftIO . threadDelay $ 60 * 1000 * 1000  -- 60 seconds
  where
    feedUrl = "https://hackage.haskell.org/packages/recent.rss"

    parseCrates = parseXMLDoc >=> elementToRSS >=> parseFeed
    parseFeed = traverse parseItem . rssItems . rssChannel
    parseItem item = do
        (name, version) <- parseTitle =<< rssItemTitle item
        (_, description) <- Text.breakOnEnd "<p>" . Text.pack <$>
            rssItemDescription item
        return $ Crate name version description
    parseTitle title
        | [name, version] <- Text.words $ Text.pack title = Just (name, version)
        | otherwise = Nothing

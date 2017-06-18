{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.Foldable
import Data.Function
import Data.List
import Data.Monoid
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Connection
import Network.HTTP.Client
import System.Clock
import System.Timeout
import Text.RSS.Import
import Text.RSS.Syntax
import Text.XML.Light.Input

import Hircine

import Utils


main :: IO ()
main = do
    crateMap <- newIORef defaultCrateMap
    channelIdle <- newIORef =<< getMonotonicTime
    newCrates <- newChan
    startBot params myNick $ \channel man -> forever $ do
        Message origin command <- receive
        return command
            ? handlePing
            ? handleQuit (== "lfairy") origin
            ? (\(Mode nick _) ->
                when (nick == myNick) $ do
                    send $ Join [channel] Nothing
                    _ <- fork . liftIO $ checkNewCrates crateMap newCrates man
                    _ <- fork $ notifyNewCrates newCrates channelIdle channel
                    send $ PrivMsg [channel] "\x01\&ACTION Hello, world!\x01"
                    return () )
            ? (\(PrivMsg targets _) ->
                when (channel `elem` targets) $ liftIO $ do
                    currentTime <- getMonotonicTime
                    atomicWriteIORef channelIdle $! currentTime + channelIdleDelay )
  where
    myNick = "hackagebot"
    params = ConnectionParams
        { connectionHostname = "chat.freenode.net"
        , connectionPort = 6697
        , connectionUseSecure = Just def
        , connectionUseSocks = Nothing
        }


checkNewCrates :: IORef CrateMap -> Chan Crate -> Manager -> IO a
checkNewCrates crateMap newCrates man = forever $ do
    changedCrates <- fetchAndUpdateCrateMap feedUrl parseCrates crateMap man
    writeList2Chan newCrates changedCrates
    threadDelay $ 60 * 1000 * 1000  -- 60 seconds
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


notifyNewCrates :: Chan Crate -> IORef TimeSpec -> ByteString -> Hircine ()
notifyNewCrates newCrates channelIdle channel = start
  where
    start = do
        firstCrate <- liftIO $ readChan newCrates
        cratesToSend <- liftIO $ newIORef [firstCrate]
        startTime <- liftIO getMonotonicTime
        loop startTime (startTime + notifyDelay) cratesToSend

    loop currentTime notifyTime cratesToSend = do
        channelIdleTime <- liftIO $ readIORef channelIdle
        let timeToWait = toMicroSecs $ max notifyTime channelIdleTime - currentTime
        if timeToWait > 0
            then do
                _ <- liftIO $ timeout timeToWait $ forever $ do
                    crate <- readChan newCrates
                    -- IORef operations are not interruptible, so there is no
                    -- threat of data loss here
                    modifyIORef' cratesToSend (crate :)
                currentTime' <- liftIO getMonotonicTime
                loop currentTime' notifyTime cratesToSend
            else do
                messages <- liftIO $ summarizeCrates <$> readIORef cratesToSend
                buffer . for_ messages $ send . PrivMsg [channel] . Text.encodeUtf8
                start


summarizeCrates :: [Crate] -> [Text]
summarizeCrates crates
    | length crates < 3 = showIndividually crates  -- <3
    | otherwise = showCompactly crates
  where
    showIndividually = map (showCrate baseUrl)
    baseUrl = "https://hackage.haskell.org/package/"

    showCompactly = showCompactly' . splitAt 4 . shuffleCrates
    showCompactly' (former, latter) = [firstLine, secondLine]
      where
        firstLine = Text.intercalate ", " $
            map showCrateNameVersion former ++ andMore
        andMore
            | null latter = []
            | otherwise = ["… and " <> Text.pack (show (length latter)) <> " more"]
        secondLine = " → https://hackage.haskell.org/packages/recent"


-- | Shuffle the list of packages such that those with similar names are spaced
-- farther apart.
--
-- This avoids the \"amazonka problem\", where a flurry of published packages
-- from a single framework ends up crowding out independent ones.
shuffleCrates :: [Crate] -> [Crate]
shuffleCrates
    = concat . transpose . groupBy ((==) `on` crateFamily) . sortOn crateName
  where
    crateFamily = Text.takeWhile (/= '-') . crateName


channelIdleDelay :: TimeSpec
channelIdleDelay = TimeSpec { sec = 60, nsec = 0 }

notifyDelay :: TimeSpec
notifyDelay = TimeSpec { sec = 5 * 60, nsec = 0 }


getMonotonicTime :: IO TimeSpec
getMonotonicTime = getTime Monotonic

toMicroSecs :: TimeSpec -> Int
toMicroSecs = ceiling . (/ (1000 :: Rational)) . fromInteger . toNanoSecs

{-# LANGUAGE OverloadedStrings #-}


import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Monoid
import Network.Simple.TCP (connect)
import System.Posix.Env.ByteString
import System.IO

import Hircine


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    secret <- getEnv' "BRIGITTE_SECRET"
    connect "irc.mozilla.org" "6667" $ \(sock, addr) -> do
        putStrLn $ "Connected to " ++ show addr
        (is, os) <- socketToIRCStreams sock
        runHircine (logMessages $ bot secret) is os
            `finally` putStrLn "Closing"


bot :: ByteString -> Hircine ()
bot secret = do
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
                        | nick `elem` ["lfairy", "nrc"] && msg == "quit" ->
                            send $ Quit (Just $ "kicked by " <> nick)
                    _ -> return () )
            ? (\(Command code _) ->
                when (code == "900") $ do
                    send $ Join ["#rust-bots"] []
                    return () )


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

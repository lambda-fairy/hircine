{-# LANGUAGE OverloadedStrings #-}

module Hircine.Command where


import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe

import Hircine.Core


class IsCommand a where
    fromCommand :: Command -> Maybe a
    toCommand :: a -> Command


fromMessage :: IsCommand a => Message -> Maybe (Msg a)
fromMessage (Message origin command) = Message origin <$> fromCommand command

toMessage :: IsCommand a => Msg a -> Message
toMessage = fmap toCommand


instance IsCommand Command where
    fromCommand = Just
    toCommand = id


instance (IsCommand a, IsCommand b) => IsCommand (Either a b) where
    fromCommand c = Left <$> fromCommand c <|> Right <$> fromCommand c
    toCommand = either toCommand toCommand


type Bytes = ByteString


data Join = Join ![Bytes] ![Bytes]
    deriving (Read, Show)

instance IsCommand Join where
    fromCommand (Command "JOIN" [channels, keys]) = Just $ Join (B.split ',' channels) (B.split ',' keys)
    fromCommand (Command "JOIN" [channels]) = Just $ Join (B.split ',' channels) []
    fromCommand _ = Nothing

    toCommand (Join channels []) = Command "JOIN" [B.intercalate "," channels]
    toCommand (Join channels keys) = Command "JOIN" [B.intercalate "," channels, B.intercalate "," keys]


data Nick = Nick !Bytes
    deriving (Read, Show)

instance IsCommand Nick where
    fromCommand (Command "NICK" [nick]) = Just $ Nick nick
    fromCommand _ = Nothing

    toCommand (Nick nick) = Command "NICK" [nick]


data Notice = Notice ![Bytes] !Bytes
    deriving (Read, Show)

instance IsCommand Notice where
    fromCommand (Command "NOTICE" [targets, message]) = Just $ Notice (B.split ',' targets) message
    fromCommand _ = Nothing

    toCommand (Notice targets message) = Command "NOTICE" [B.intercalate "," targets, message]


data Pass = Pass !Bytes
    deriving (Read, Show)

instance IsCommand Pass where
    fromCommand (Command "PASS" [pass]) = Just $ Pass pass
    fromCommand _ = Nothing

    toCommand (Pass pass) = Command "PASS" [pass]


data Ping = Ping !Bytes !(Maybe Bytes)
    deriving (Read, Show)

instance IsCommand Ping where
    fromCommand (Command "PING" [server1]) = Just $ Ping server1 Nothing
    fromCommand (Command "PING" [server1, server2]) = Just $ Ping server1 (Just server2)
    fromCommand _ = Nothing

    toCommand (Ping server1 server2) = Command "PING" $ server1 : maybeToList server2


data Pong = Pong !Bytes !(Maybe Bytes)
    deriving (Read, Show)

instance IsCommand Pong where
    fromCommand (Command "PONG" [server1]) = Just $ Pong server1 Nothing
    fromCommand (Command "PONG" [server1, server2]) = Just $ Pong server1 (Just server2)
    fromCommand _ = Nothing

    toCommand (Pong server1 server2) = Command "PONG" $ server1 : maybeToList server2


data PrivMsg = PrivMsg ![Bytes] !Bytes
    deriving (Read, Show)

instance IsCommand PrivMsg where
    fromCommand (Command "PRIVMSG" [targets, message]) = Just $ PrivMsg (B.split ',' targets) message
    fromCommand _ = Nothing

    toCommand (PrivMsg targets message) = Command "PRIVMSG" [B.intercalate "," targets, message]


data Quit = Quit !(Maybe Bytes)
    deriving (Read, Show)

instance IsCommand Quit where
    fromCommand (Command "QUIT" []) = Just $ Quit Nothing
    fromCommand (Command "QUIT" [message]) = Just $ Quit (Just message)
    fromCommand _ = Nothing

    toCommand (Quit message) = Command "QUIT" $ maybeToList message


data User = User !Bytes !Bytes
    deriving (Read, Show)

instance IsCommand User where
    fromCommand (Command "USER" [user, _, _, realname]) = Just $ User user realname
    fromCommand _ = Nothing

    toCommand (User user realname) = Command "USER" [user, "0", "*", realname]

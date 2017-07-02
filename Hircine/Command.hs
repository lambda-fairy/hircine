{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides a typed interface for handling IRC commands.
-- It lets you work in terms of e.g. 'Join' or 'PrivMsg' values instead
-- of having to match on the raw 'Command' type.

module Hircine.Command (

    -- * Parsing and un-parsing commands
    IsCommand(..),
    Bytes,

    -- * Command types
    pattern Join,
    pattern Mode,
    pattern Nick,
    pattern Notice,
    pattern Pass,
    pattern Ping,
    pattern Pong,
    pattern PrivMsg,
    pattern Quit,
    pattern User,

    -- * Low-level machinery
    -- | This section contains the low-level machinery that makes this
    -- module work. You shouldn't need to deal with these routines
    -- unless you're adding a new command.
    ParsedCommand(..),
    ParamParser,
    runParamParser,
    IsParams(..),
    CommaSep(..),
    unCommaSep,

    ) where


import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Proxy
import Data.String
import GHC.TypeLits

import Hircine.Core


-- | Represents a type that can be converted to and from a raw 'Command'.
class IsCommand a where
    fromCommand :: Command -> Maybe a
    toCommand :: a -> Command


instance IsCommand Command where
    fromCommand = Just
    toCommand = id


-- | Represents a validated IRC command.
newtype ParsedCommand (method :: Symbol) params = ParsedCommand params
    deriving Show

instance forall method params. (KnownSymbol method, IsParams params) => IsCommand (ParsedCommand method params) where
    fromCommand (Command method' params')
        | method == method' = ParsedCommand <$> runParamParser parseParams params'
        | otherwise = Nothing
      where
        method = fromString $ symbolVal (Proxy :: Proxy method)
    toCommand (ParsedCommand params) = Command method (renderParams params)
      where
        method = fromString $ symbolVal (Proxy :: Proxy method)


-- | Convenient synonym for 'ByteString'.
type Bytes = ByteString


type ParamParser = StateT [Bytes] Maybe

runParamParser :: Alternative f => ParamParser a -> [Bytes] -> f a
runParamParser parse params'
    | Just (params, []) <- runStateT parse params' = pure params
    | otherwise = empty


class IsParams a where
    parseParams :: ParamParser a
    renderParams :: a -> [Bytes]

instance IsParams a => IsParams [a] where
    parseParams = many parseParams
    renderParams = concatMap renderParams

instance IsParams () where
    parseParams = pure ()
    renderParams _ = []

instance (IsParams a, IsParams b) => IsParams (a, b) where
    parseParams = (,) <$> parseParams <*> parseParams
    renderParams (a, b) = renderParams a ++ renderParams b

instance (IsParams a, IsParams b, IsParams c) => IsParams (a, b, c) where
    parseParams = (,,) <$> parseParams <*> parseParams <*> parseParams
    renderParams (a, b, c) = renderParams a ++ renderParams b ++ renderParams c

instance (IsParams a, IsParams b, IsParams c, IsParams d) => IsParams (a, b, c, d) where
    parseParams = (,,,) <$> parseParams <*> parseParams <*> parseParams <*> parseParams
    renderParams (a, b, c, d) = renderParams a ++ renderParams b ++ renderParams c ++ renderParams d

instance (IsParams a, IsParams b, IsParams c, IsParams d, IsParams e) => IsParams (a, b, c, d, e) where
    parseParams = (,,,,) <$> parseParams <*> parseParams <*> parseParams <*> parseParams <*> parseParams
    renderParams (a, b, c, d, e) = renderParams a ++ renderParams b ++ renderParams c ++ renderParams d ++ renderParams e

instance IsParams ByteString where
    parseParams = StateT uncons
    renderParams x = [x]

newtype CommaSep a = CommaSep [a]
    deriving Show

-- Written separately to make the Show instance prettier
unCommaSep :: CommaSep a -> [a]
unCommaSep (CommaSep a) = a

instance IsParams a => IsParams (CommaSep a) where
    parseParams = do
        pieces <- B.split ',' <$> StateT uncons
        runParamParser (CommaSep <$> many parseParams) pieces
    renderParams (CommaSep xs) = [B.intercalate "," $ concatMap renderParams xs]

instance IsParams a => IsParams (Maybe a) where
    parseParams = optional parseParams
    renderParams = foldMap renderParams


pattern Join :: [Bytes] -> Maybe [Bytes] -> ParsedCommand "JOIN" (CommaSep Bytes, Maybe (CommaSep Bytes))
pattern Join channels keys <- ParsedCommand (CommaSep channels, fmap unCommaSep -> keys)
  where
    Join channels keys = ParsedCommand (CommaSep channels, CommaSep <$> keys)

pattern Mode :: Bytes -> [Bytes] -> ParsedCommand "MODE" (Bytes, [Bytes])
pattern Mode channelOrNick args = ParsedCommand (channelOrNick, args)

pattern Nick :: Bytes -> ParsedCommand "NICK" Bytes
pattern Nick nick = ParsedCommand nick

pattern Notice :: [Bytes] -> Bytes -> ParsedCommand "NOTICE" (CommaSep Bytes, Bytes)
pattern Notice targets message = ParsedCommand (CommaSep targets, message)

pattern Pass :: Bytes -> ParsedCommand "PASS" Bytes
pattern Pass pass = ParsedCommand pass

pattern Ping :: Bytes -> Maybe Bytes -> ParsedCommand "PING" (Bytes, Maybe Bytes)
pattern Ping server1 server2 = ParsedCommand (server1, server2)

pattern Pong :: Bytes -> Maybe Bytes -> ParsedCommand "PONG" (Bytes, Maybe Bytes)
pattern Pong server1 server2 = ParsedCommand (server1, server2)

pattern PrivMsg :: [Bytes] -> Bytes -> ParsedCommand "PRIVMSG" (CommaSep Bytes, Bytes)
pattern PrivMsg targets message = ParsedCommand (CommaSep targets, message)

pattern Quit :: Maybe Bytes -> ParsedCommand "QUIT" (Maybe Bytes)
pattern Quit message = ParsedCommand message

pattern User :: Bytes -> Bytes -> ParsedCommand "USER" (Bytes, Bytes, Bytes, Bytes)
pattern User user realname <- ParsedCommand (user, _, _, realname)
  where
    User user realname = ParsedCommand (user, "0", "*", realname)

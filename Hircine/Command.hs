{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Hircine.Command where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Data.Maybe
import Data.Proxy
import Data.String
import GHC.TypeLits

import Hircine.Core


class IsCommand a where
    fromCommand :: Command -> Maybe a
    toCommand :: a -> Command


(?) :: (Monad m, IsCommand a) => m Command -> (a -> m ()) -> m Command
m ? k = do
    c <- m
    mapM_ k $ fromCommand c
    return c
infixl 2 ?


instance IsCommand Command where
    fromCommand = Just
    toCommand = id


newtype ParsedCommand (method :: Symbol) (params :: [*]) = ParsedCommand (List params)

instance forall method params. (KnownSymbol method, IsParams params) => IsCommand (ParsedCommand method params) where
    fromCommand (Command method' params)
        | method /= method' = Nothing
        | otherwise = ParsedCommand <$> parseParams params
      where
        method = fromString $ symbolVal (Proxy :: Proxy method)
    toCommand (ParsedCommand params) = Command method (renderParams params)
      where
        method = fromString $ symbolVal (Proxy :: Proxy method)


data List (params :: [*]) where
    Nil :: List '[]
    (:-) :: a -> List b -> List (a ': b)
infixr 5 :-


class IsParams (params :: [*]) where
    parseParams :: [Bytes] -> Maybe (List params)
    renderParams :: List params -> [Bytes]

instance IsParams '[] where
    parseParams [] = Just Nil
    parseParams _ = Nothing
    renderParams Nil = []

instance (IsParam a, IsParams b) => IsParams (a ': b) where
    parseParams (x : xs) = (:-) <$> parseParam x <*> parseParams xs
    parseParams _ = Nothing
    renderParams (x :- xs) = toList (renderParam x) ++ renderParams xs


class IsParam a where
    parseParam :: Bytes -> Maybe a
    renderParam :: a -> Maybe Bytes

instance IsParam ByteString where
    parseParam = Just
    renderParam = Just

newtype CommaSep a = CommaSep [a]
    deriving (Read, Show)

instance IsParam a => IsParam (CommaSep a) where
    parseParam = fmap CommaSep . traverse parseParam . B.split ','
    renderParam (CommaSep xs) = Just $ B.intercalate "," $ mapMaybe renderParam xs

instance IsParam a => IsParam (Maybe a) where
    parseParam "" = Just Nothing
    parseParam x = Just <$> parseParam x
    renderParam x = x >>= renderParam


type Bytes = ByteString


pattern Join :: [Bytes] -> [Bytes] -> ParsedCommand "JOIN" '[CommaSep Bytes, Maybe (CommaSep Bytes)]
pattern Join channels keys <-
    ParsedCommand (
        CommaSep channels :-
        ((\keys' -> case keys' of
            Just (CommaSep xs) -> xs
            Nothing -> []
        ) -> keys) :-
        Nil)
  where
    Join channels keys = ParsedCommand (CommaSep channels :- packKeys keys :- Nil)
      where
        packKeys [] = Nothing
        packKeys xs = Just (CommaSep xs)

pattern Nick :: Bytes -> ParsedCommand "NICK" '[Bytes]
pattern Nick nick = ParsedCommand (nick :- Nil)

pattern Notice :: [Bytes] -> Bytes -> ParsedCommand "NOTICE" '[CommaSep Bytes, Bytes]
pattern Notice targets message = ParsedCommand (CommaSep targets :- message :- Nil)

pattern Pass :: Bytes -> ParsedCommand "PASS" '[Bytes]
pattern Pass pass = ParsedCommand (pass :- Nil)

pattern Ping :: Bytes -> Maybe Bytes -> ParsedCommand "PING" '[Bytes, Maybe Bytes]
pattern Ping server1 server2 = ParsedCommand (server1 :- server2 :- Nil)

pattern Pong :: Bytes -> Maybe Bytes -> ParsedCommand "PONG" '[Bytes, Maybe Bytes]
pattern Pong server1 server2 = ParsedCommand (server1 :- server2 :- Nil)

pattern PrivMsg :: [Bytes] -> Bytes -> ParsedCommand "PRIVMSG" '[CommaSep Bytes, Bytes]
pattern PrivMsg targets message = ParsedCommand (CommaSep targets :- message :- Nil)

pattern Quit :: Maybe Bytes -> ParsedCommand "QUIT" '[Maybe Bytes]
pattern Quit message = ParsedCommand (message :- Nil)

pattern User :: Bytes -> Bytes -> ParsedCommand "USER" '[Bytes, Bytes, Bytes, Bytes]
pattern User user realname <- ParsedCommand (user :- _ :- _ :- realname :- Nil) where
    User user realname = ParsedCommand (user :- "0" :- "*" :- realname :- Nil)

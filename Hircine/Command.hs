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
import Data.Void
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


newtype ParsedCommand (method :: Symbol) (params :: [*]) extra = ParsedCommand (List params) (Maybe extra)

instance forall method params extra. (KnownSymbol method, IsParams params, IsParam extra) => IsCommand (ParsedCommand method params extra) where
    fromCommand (Command method' params)
        | method /= method' = Nothing
        | otherwise = ParsedCommand <$> parseParams params <*> pure Nothing
            <|> ParsedCommand <$> parseParams (init params) <*> (Just <$> parseParam (last params))
      where
        method = fromString $ symbolVal (Proxy :: Proxy method)
    toCommand (ParsedCommand params extra) = Command method (renderParams params ++ toList (renderParam <$> extra))
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
    renderParams (x :- xs) = renderParam x : renderParams xs


class IsParam a where
    parseParam :: Bytes -> Maybe a
    renderParam :: a -> Bytes

instance IsParam ByteString where
    parseParam = Just
    renderParam = id

instance IsParam Void where
    parseParam _ = Nothing
    renderParam = absurd

newtype CommaSep a = CommaSep [a]
    deriving (Read, Show)

instance IsParam a => IsParam (CommaSep a) where
    parseParam = fmap CommaSep . traverse parseParam . B.split ','
    renderParam (CommaSep xs) = B.intercalate "," $ map renderParam xs


type Bytes = ByteString


pattern Join :: [Bytes] -> [Bytes] -> ParsedCommand "JOIN" '[CommaSep Bytes] (CommaSep Bytes)
pattern Join channels keys <-
    ParsedCommand (CommaSep channels :- Nil)
        ((\keys' -> case keys' of
            Just (CommaSep xs) -> xs
            Nothing -> []
        ) -> keys)
  where
    Join channels keys = ParsedCommand (CommaSep channels :- Nil) (packKeys keys)
      where
        packKeys [] = Nothing
        packKeys xs = Just (CommaSep xs)

pattern Nick :: Bytes -> ParsedCommand "NICK" '[Bytes] Void
pattern Nick nick = ParsedCommand (nick :- Nil) Nothing

pattern Notice :: [Bytes] -> Bytes -> ParsedCommand "NOTICE" '[CommaSep Bytes, Bytes] Void
pattern Notice targets message = ParsedCommand (CommaSep targets :- message :- Nil) Nothing

pattern Pass :: Bytes -> ParsedCommand "PASS" '[Bytes] Void
pattern Pass pass = ParsedCommand (pass :- Nil) Nothing

pattern Ping :: Bytes -> Maybe Bytes -> ParsedCommand "PING" '[Bytes] Bytes
pattern Ping server1 server2 = ParsedCommand (server1 :- Nil) server2

pattern Pong :: Bytes -> Maybe Bytes -> ParsedCommand "PONG" '[Bytes] Bytes
pattern Pong server1 server2 = ParsedCommand (server1 :- Nil) server2

pattern PrivMsg :: [Bytes] -> Bytes -> ParsedCommand "PRIVMSG" '[CommaSep Bytes, Bytes] Void
pattern PrivMsg targets message = ParsedCommand (CommaSep targets :- message :- Nil) Nothing

pattern Quit :: Maybe Bytes -> ParsedCommand "QUIT" '[] Bytes
pattern Quit message = ParsedCommand Nil Message

pattern User :: Bytes -> Bytes -> ParsedCommand "USER" '[Bytes, Bytes, Bytes, Bytes] Void
pattern User user realname <- ParsedCommand (user :- _ :- _ :- realname :- Nil) Nothing where
    User user realname = ParsedCommand (user :- "0" :- "*" :- realname :- Nil) Nothing

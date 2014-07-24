{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Simple IRC message parser.
--
-- The grammar accepted here is slightly different to that specified in
-- the RFC, for simplicity and compatibility.
--
-- This module also includes 'IsString' instances for 'Message',
-- 'Prefix', and 'Command', which accept the same syntax.

module Hircine.Parser
    ( parseMessage
    ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Attoparsec
import qualified Data.ByteString as B
import Data.String (IsString(fromString))
import Data.Word

import Hircine.Types


-- | Parse a message. The input string should not have a trailing CRLF.
parseMessage :: Bytes -> Either String Message
parseMessage = parseOnly $ space *> message <* endOfInput


instance IsString Message where
    fromString = fromStringDefault "message" message

instance IsString Prefix where
    fromString = fromStringDefault "prefix" prefix

instance IsString Command where
    fromString = fromStringDefault "command" command

fromStringDefault :: String -> Parser a -> (String -> a)
fromStringDefault ty p s = case parseOnly (p <* endOfInput) $ fromString s of
    Left err -> error $ concat ["Invalid ", ty, ": ", show s, " (", err, ")"]
    Right res -> res


message :: Parser Message
message = Message
    <$> optional prefix
    <*> command
    <*> params
    <*> optional trailing


prefix :: Parser Prefix
prefix = token . (<?> "prefix") $
    string ":" *> (user <|> FromServer <$> ident)
  where
    user = FromUser
        <$> ident
        <*> (string "!" *> ident)
        <*> (string "@" *> ident)


command :: Parser Command
command = token . (<?> "command") $
        Command <$> takeWhile1 (inClass "A-Za-z")
    <|> StatusCode <$> digit <*> digit <*> digit


params :: Parser [Bytes]
params = many . token . (<?> "parameters") $
    B.cons <$> satisfy (notInClass ("\x00-\x20:"))
        <*> takeWhile (notInClass "\x00-\x20")


trailing :: Parser Bytes
trailing = string ":" *> takeWhile (notInClass "\r\n")


digit :: Parser Word8
digit = subtract 0x30 <$> satisfy (inClass "0-9")

ident :: Parser Bytes
ident = takeWhile1 (inClass "-./0-9A-Z[\\]^_`a-z{|}~")

token :: Parser a -> Parser a
token = (<* space)

space :: Parser ()
space = skipWhile (== 0x20)

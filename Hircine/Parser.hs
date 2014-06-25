{-# LANGUAGE OverloadedStrings #-}

-- | Simple IRC message parser.
--
-- The grammar accepted here is slightly different to that specified in
-- the RFC, for simplicity and compatibility.

module Hircine.Parser
    ( parseMessage
    ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Attoparsec
import qualified Data.ByteString as B
import Data.Word

import Hircine.Types


-- | Parse a message. The input string should not have a trailing CRLF.
parseMessage :: Bytes -> Either String Message
parseMessage = parseOnly (message <* endOfInput)


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
        Command <$> takeWhile (inClass "A-Za-z")
    <|> StatusCode <$> digit <*> digit <*> digit


params :: Parser [Bytes]
params = many . token . (<?> "parameters") $
    B.cons <$> satisfy (notInClass ("\x00-\x20:"))
        <*> takeWhile (notInClass "\x00-\x20")


trailing :: Parser Bytes
trailing = string ":" *> takeByteString


digit :: Parser Word8
digit = subtract 0x30 <$> satisfy (inClass "0-9")

ident :: Parser Bytes
ident = takeWhile (inClass "-./0-9A-Z[\\]^_`a-z{|}~")

token :: Parser a -> Parser a
token = (<* space)

space :: Parser ()
space = skipWhile (== 0x20)

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hircine.Core.Parser (
    parseMessage
    ) where


import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.String
import Data.Word

import Hircine.Core.Types


-- | Parse a message. The input string should represent exactly one
-- message, with no trailing newlines.
parseMessage :: ByteString -> Either String Message
parseMessage = parseOnly $ message <* skipSpace <* endOfInput


instance IsString Method where
    fromString s = case parseOnly (method <* endOfInput) $ B.pack s of
        Left _ -> error $ "invalid method " ++ show s
        Right r -> r


message :: Parser Message
message = Message <$> optional (origin <* skipSpace1) <*> command


origin :: Parser Origin
origin = ":" *> (user <|> FromServer <$> takeWhile1 (not . isSpace))
  where
    user = FromUser
        <$> takeWhile1 (notInClass " !@")
        <* "!"
        <*> takeWhile1 (notInClass " @")
        <* "@"
        <*> takeWhile1 (/= ' ')


command :: Parser Command
command = Command <$> method <*> params


method :: Parser Method
method = Textual . B.map toUpper <$> takeWhile1 isAlpha_ascii
    <|> Numeric <$> digit' <*> digit' <*> digit'


params :: Parser [ByteString]
params = many (skipSpace1 *> param)
  where
    param = do
        c <- anyChar
        if c == ':'
            then takeByteString
            else B.cons c <$> takeWhile (not . isSpace)


digit' :: Parser Word8
digit' = read . (:[]) <$> digit


skipSpace1 :: Parser ()
skipSpace1 = void $ takeWhile1 isSpace

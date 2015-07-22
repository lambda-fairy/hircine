{-# LANGUAGE OverloadedStrings #-}

module Hircine.Core.Types where


import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Traversable as T
import Data.Word
import Prelude  -- GHC 7.10


-- | A raw message.
type Message = Msg Command


-- | An IRC message consists of an optional prefix, along with a message
-- body.
data Msg body = Message {
    msgOrigin :: !(Maybe Origin),
    msgCommand :: !body
    } deriving (Eq, Read, Show)

instance Functor Msg where
    fmap f (Message origin command) = Message origin (f command)

instance F.Foldable Msg where
    foldMap f (Message _ command) = f command

instance T.Traversable Msg where
    traverse f (Message origin command) = Message origin <$> f command


-- | A message can originate from a fellow user, or the server itself.
--
-- If the former, the prefix must specify the nickname, username and
-- host address. Technically only the nickname is required, but in
-- practice servers always supply all three fields.
--
data Origin = FromServer !ByteString | FromUser !ByteString !ByteString !ByteString
    deriving (Eq, Ord, Read, Show)


-- | An IRC command, along with its parameters.
--
-- Invariant: Only the final parameter may contain whitespace, or start
-- with a colon (@:@).
--
data Command = Command {
    cmdMethod :: !Method,
    cmdParams :: ![ByteString]
    } deriving (Eq, Read, Show)


-- | A command is specified by either a sequence of uppercase ASCII
-- letters, or a three digit code.
data Method = Textual !ByteString | Numeric !Word8 !Word8 !Word8
    deriving (Eq, Ord, Read, Show)


-- | Render a message into a string.
renderMessage :: Message -> ByteString
renderMessage (Message origin command)
    = F.foldMap renderOrigin origin <> renderCommand command
  where
    renderOrigin (FromServer host) = ":" <> host <> " "
    renderOrigin (FromUser nick user host)
        = mconcat [":", nick, "!", user, "@", host, " "]

renderCommand :: Command -> ByteString
renderCommand (Command method params)
    = renderMethod method <> renderParams params
  where
    renderMethod (Textual name) = name
    renderMethod (Numeric x y z) = B.pack (concatMap show [x, y, z])

    renderParams ps = case ps of
        [] -> ""
        [p] | shouldEscape p -> " :" <> p
            | otherwise -> " " <> p
        (p:ps')
            | isInvalidNonFinalParam p -> error $
                "renderCommand: invalid parameter: " ++ show p
            | otherwise -> " " <> p <> renderParams ps'

    isInvalidNonFinalParam p
        = B.null p || B.head p == ':' || B.any isSpace p

    shouldEscape p
        = B.null p || B.any (\c -> c == ':' || isSpace c) p


showMessage :: Message -> String
showMessage = Text.unpack . decode . renderMessage

showCommand :: Command -> String
showCommand = Text.unpack . decode . renderCommand


testMessage :: Message
testMessage = Message origin $ Command method params
  where
    origin = Just $ FromUser "lfairy" "ducks" "geese"
    method = Textual "PRIVMSG"
    params = ["#haskell", "Hello, world!"]


-- | Try to decode using UTF-8, then Latin-1.
decode :: ByteString -> Text
decode s = case Text.decodeUtf8' s of
    Right s' -> s'
    Left _ -> Text.decodeLatin1 s

-- | Alias for 'Text.encodeUtf8'.
encode :: Text -> ByteString
encode = Text.encodeUtf8

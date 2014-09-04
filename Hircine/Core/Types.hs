{-# LANGUAGE OverloadedStrings #-}

module Hircine.Core.Types where


import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable (foldMap)
import Data.Monoid
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Data.Word


-- | A raw message.
type Message = Msg Command


-- | An IRC message consists of an optional prefix, along with a
-- message body.
data Msg body = Message {
    msgOrigin :: !(Maybe Origin),
    msgCommand :: !body
    } deriving (Eq, Read, Show)

instance Functor Msg where
    fmap f (Message origin command) = Message origin (f command)


-- | A message can originate from a fellow user, or the server itself.
--
-- If the former, the prefix must specify the nickname, username and
-- host address. Technically only the nickname is required, but in practice
-- servers always supply all three fields.
--
data Origin = Server !Bytes | User !Bytes !Bytes !Bytes
    deriving (Eq, Ord, Read, Show)


data Command = Command {
    cmdMethod :: !Method,
    cmdParams :: ![Bytes]
    } deriving (Eq, Read, Show)


-- | A command is specified by either a sequence of uppercase ASCII
-- letters, or a three digit code.
data Method = Textual !Bytes | Numeric !Word8 !Word8 !Word8
    deriving (Eq, Ord, Read, Show)


-- | Render a message into a string.
renderMessage :: Message -> Bytes
renderMessage (Message origin command)
    = foldMap renderOrigin origin <> renderCommand command
  where
    renderOrigin (Server host) = ":" <> host <> " "
    renderOrigin (User nick user host)
        = mconcat [":", nick, "!", user, "@", host, " "]

renderCommand :: Command -> Bytes
renderCommand (Command method params)
    = renderMethod method <> renderParams params
  where
    renderMethod (Textual name) = name
    renderMethod (Numeric x y z) = B.pack (concatMap show [x, y, z])

    renderParams ps = case ps of
        [] -> ""
        [p] | shouldEscape p -> " :" <> p
            | otherwise -> " " <> p
        (p:ps') -> " " <> p <> renderParams ps'

    shouldEscape p = B.null p || B.any (\c -> c == ':' || isSpace c) p


testMessage :: Message
testMessage = Message origin $ Command method params
  where
    origin = Just $ User "lfairy" "ducks" "geese"
    method = Textual "PRIVMSG"
    params = ["#haskell", "Hello, world!"]


-- | Try to decode using UTF-8, then Latin-1.
decode :: Bytes -> Text
decode s = case Text.decodeUtf8' s of
    Right s' -> s'
    Left _ -> Text.decodeLatin1 s

-- | Alias for 'Text.encodeUtf8'.
encode :: Text -> Bytes
encode = Text.encodeUtf8


-- | Convenient synonym for 'ByteString'.
type Bytes = ByteString

{-# LANGUAGE OverloadedStrings #-}

module Hircine.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable (foldMap)
import Data.Monoid
import Data.Word


data Connection = Connection {

    -- | Read a single line from the server, stripping the trailing CRLF.
    recvLine :: IO Bytes,

    -- | Send a single line to the server, appending a trailing CRLF.
    sendLine :: Bytes -> IO ()

    }


data Message = Message
    { msgPrefix :: Maybe Prefix
    , msgCommand :: Command
    , msgParams :: [Bytes]
    , msgTrailing :: Maybe Bytes }
    deriving (Eq, Show)


-- | A message can originate from a fellow user, or the server itself.
--
-- If the former, the prefix must specify the nickname, username and
-- host address.
--
data Prefix = FromUser Bytes Bytes Bytes | FromServer Bytes
    deriving (Eq, Show)


-- | A command can be either a sequence of uppercase ASCII letters, or
-- a three digit code.
data Command = Command Bytes | StatusCode Word8 Word8 Word8
    deriving (Eq, Show)


testMessage :: Message
testMessage = Message prefix command params trailing
  where
    prefix = Just $ FromUser "lfairy" "ducks" "geese"
    command = Command "PRIVMSG"
    params = ["#haskell"]
    trailing = Just "Hello, world!"


-- | Serialize a message, excluding the trailing CRLF.
showMessage :: Message -> Bytes
showMessage (Message prefix command params trailing)
    = foldr1 combine
        [ foldMap showPrefix prefix
        , showCommand command
        , B.intercalate " " params
        , foldMap (":" <>) trailing ]
  where
    combine "" b = b
    combine a "" = a
    combine a b = a <> " " <> b

    showPrefix (FromServer host) = ":" <> host
    showPrefix (FromUser nick user host)
        = mconcat [nick, "!", user, "@", host]

    showCommand (Command cmd) = cmd
    showCommand (StatusCode x y z) = B.pack (concatMap show [x, y, z])


-- | Convert a nickname to lowercase, according to IRC rules.
--
-- From RFC 1459, section 2.2:
--
-- "Because of IRC's scandanavian origin, the characters {}| are
-- considered to be the lower case equivalents of the characters []\,
-- respectively. This is a critical issue when determining the
-- equivalence of two nicknames."
--
caseFoldNick :: Bytes -> Bytes
caseFoldNick = B.map fold
  where
    fold c = case c of
        '[' -> '{'
        '\\' -> '|'
        ']' -> '}'
        _ -> toLower c


-- | Convenient synonym for 'ByteString'.
type Bytes = ByteString

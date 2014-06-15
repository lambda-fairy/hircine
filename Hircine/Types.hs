{-# LANGUAGE OverloadedStrings #-}

module Hircine.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable (foldMap)
import Data.Monoid


data Message = Message
    { msgPrefix :: Maybe Prefix
    , msgCommand :: Command
    , msgParams :: [Bytes]
    , msgTrailing :: Maybe Bytes }
    deriving (Eq, Show)


data Prefix = PrefixServer Bytes | PrefixUser User
    deriving (Eq, Show)


-- | A command can be either a sequence of uppercase ASCII letters, or
-- a three digit code.
data Command = Command Bytes | CommandNumber Int
    deriving (Eq, Show)


-- | A user specification includes a nickname, a username and a host
-- address. The latter two may be omitted.
data User = User
    { userNick :: Bytes
    , userUser :: Maybe Bytes
    , userHost :: Maybe Bytes }
    deriving (Eq, Show)


testMessage :: Message
testMessage = Message prefix command params trailing
  where
    prefix = Just . PrefixUser $ User "lfairy" (Just "ducks") (Just "geese")
    command = Command "PRIVMSG"
    params = ["#haskell"]
    trailing = Just "Hello, world!"


showMessage :: Message -> Bytes
showMessage (Message prefix command params trailing)
    = foldr1 combine
        [ foldMap showPrefix prefix
        , showCommand command
        , B.intercalate " " params
        , foldMap (":" <>) trailing ]
      <> "\r\n"
  where
    combine "" b = b
    combine a "" = a
    combine a b = a <> " " <> b

    showPrefix (PrefixServer host) = ":" <> host
    showPrefix (PrefixUser user) = ":" <> showUser user

    showUser (User nick muser mhost) = nick
        <> foldMap ("!" <>) muser
        <> foldMap ("@" <>) mhost

    showCommand (Command cmd) = cmd
    showCommand (CommandNumber n) = B.pack (show n)


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

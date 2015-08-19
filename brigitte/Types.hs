{-# LANGUAGE OverloadedStrings #-}

module Types (
    Crate(..),
    showCrate,
    CrateMap,
    defaultCrateMap,
    updateCrateMap,
    ) where


import Control.Applicative
import Data.Aeson
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text


data Crate = Crate {
    crateName :: !Text,
    crateMaxVersion :: !Text,
    crateDescription :: !Text
    } deriving (Eq, Show)

instance FromJSON Crate where
    parseJSON (Object v) = Crate
        <$> v .: "name"
        <*> v .: "max_version"
        <*> v .: "description"
    parseJSON _ = empty

showCrate :: Crate -> Text
showCrate p = Text.intercalate " – " [
    crateName p <> " " <> crateMaxVersion p,
    clipTo 80 . collapseSpaces $ crateDescription p,
    "https://crates.io/crates/" <> crateName p
    ]

clipTo :: Int -> Text -> Text
clipTo n s
    | Text.length s <= n = s
    | otherwise =
        (Text.dropWhileEnd isSpace . Text.dropWhileEnd (not . isSpace) $
            Text.take (n + 1) s)
        <> "…"

collapseSpaces :: Text -> Text
collapseSpaces = Text.unwords . Text.words


type CrateMap = Map Text (Text, Text)

defaultCrateMap :: CrateMap
defaultCrateMap = Map.empty

-- | Update the internal crate map. Return the set of crates that were
-- changed.
updateCrateMap :: [Crate] -> CrateMap -> (CrateMap, [Crate])
updateCrateMap newCrates' oldCrates = (newCrates, changedCrates)
  where
    newCrates = toCrateMap newCrates'
    changedCrates
        | Map.null oldCrates = []
        | otherwise = fromCrateMap $ Map.differenceWith
            (\new old -> if new /= old then Just new else Nothing)
            newCrates oldCrates

toCrateMap :: [Crate] -> CrateMap
toCrateMap crates = Map.fromList [
    (crateName p, (crateMaxVersion p, crateDescription p)) |
    p <- crates ]

fromCrateMap :: CrateMap -> [Crate]
fromCrateMap crates = [ Crate name vers desc |
    (name, (vers, desc)) <- Map.toList crates ]

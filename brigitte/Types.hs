{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}

module Types where


import Control.Applicative
import Control.Monad.State
import Data.Acid
import Data.Aeson
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as Text


data Crate = Crate {
    crateName :: !Text,
    crateMaxVersion :: !Text,
    crateDescription :: !Text
    } deriving (Eq, Show)

deriveSafeCopy 0 'base ''Crate

instance FromJSON Crate where
    parseJSON (Object v) = Crate
        <$> v .: "name"
        <*> v .: "max_version"
        <*> v .: "description"
    parseJSON _ = empty


showCrate :: Crate -> Text
showCrate p = Text.intercalate " – " [
    crateName p <> " " <> crateMaxVersion p,
    summarize 80 $ crateDescription p,
    "https://crates.io/crates/" <> crateName p
    ]


summarize :: Int -> Text -> Text
summarize limit = Text.unwords . unfoldr f . (,) 0 . Text.words
  where
    f (_, []) = Nothing
    f (n, (w : ws))
        | n' > limit = Just ("…", (n', []))
        | otherwise = Just (w, (n', ws))
      where
        n' = n + Text.length w


data BrigitteState = BrigitteState {
    crateMap :: !CrateMap
    } deriving Show

type CrateMap = Map Text (Text, Text)

deriveSafeCopy 0 'base ''BrigitteState


defaultBrigitteState :: BrigitteState
defaultBrigitteState = BrigitteState {
    crateMap = Map.empty
    }


-- | Update the internal crate map. Return the set of crates that were
-- changed.
updateCrates :: [Crate] -> Update BrigitteState [Crate]
updateCrates updatedCrates = do
    BrigitteState oldCrates <- get
    let changedCrates = Map.differenceWith
            (\new old -> if new /= old then Just new else Nothing)
            (toCrateMap updatedCrates) oldCrates
        newCrates = Map.union changedCrates oldCrates
    put $ BrigitteState newCrates
    return $ fromCrateMap changedCrates
  where
    toCrateMap :: [Crate] -> CrateMap
    toCrateMap crates = Map.fromList [
        (crateName p, (crateMaxVersion p, crateDescription p)) |
        p <- crates ]

    fromCrateMap :: CrateMap -> [Crate]
    fromCrateMap crates = [ Crate name vers desc |
        (name, (vers, desc)) <- Map.toList crates ]

makeAcidic ''BrigitteState ['updateCrates]

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


data Package = Package {
    packageName :: Text,
    packageMaxVersion :: Text,
    packageDescription :: Text
    } deriving (Eq, Show)

deriveSafeCopy 0 'base ''Package

instance FromJSON Package where
    parseJSON (Object v) = Package
        <$> v .: "name"
        <*> v .: "max_version"
        <*> v .: "description"
    parseJSON _ = empty


showPackage :: Package -> Text
showPackage p = Text.intercalate " – " [
    packageName p <> " " <> packageMaxVersion p,
    summarize 80 $ packageDescription p,
    "https://crates.io/crates/" <> packageName p
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
    packageMap :: !PackageMap
    } deriving Show

type PackageMap = Map Text (Text, Text)

deriveSafeCopy 0 'base ''BrigitteState


defaultBrigitteState :: BrigitteState
defaultBrigitteState = BrigitteState {
    packageMap = Map.empty
    }


-- | Update the internal package map. Return the set of packages that
-- were changed.
updatePackages :: [Package] -> Update BrigitteState [Package]
updatePackages updatedPackages = do
    BrigitteState oldPackages <- get
    let changedPackages = Map.differenceWith
            (\new old -> if new /= old then Just new else Nothing)
            (toPackageMap updatedPackages) oldPackages
        newPackages = Map.union changedPackages oldPackages
    put $ BrigitteState newPackages
    return $ fromPackageMap changedPackages
  where
    toPackageMap :: [Package] -> PackageMap
    toPackageMap packages = Map.fromList [
        (packageName p, (packageMaxVersion p, packageDescription p)) |
        p <- packages ]
    fromPackageMap :: PackageMap -> [Package]
    fromPackageMap packages = [ Package name vers desc |
        (name, (vers, desc)) <- Map.toList packages ]

makeAcidic ''BrigitteState ['updatePackages]

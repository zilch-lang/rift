{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS -Wno-name-shadowing #-}

module Rift.Config.PackageSet where

import Control.Exception (Handler (..), SomeException, catches, handle, throwIO)
import Control.Monad (when)
import Data.Foldable (fold)
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Dhall (inputFile)
import Dhall.Import (Imported (Imported))
import Dhall.Marshal.Decode (FromDhall (..), auto, constructor, field, natural, record, union, unit)
import Dhall.Marshal.Encode (ToDhall (..), encodeConstructor, encodeConstructorWith, encodeField, recordEncoder, unionEncoder, (>$<), (>*<), (>|<))
import Dhall.Src (Src)
import Dhall.TypeCheck (TypeError)
import GHC.Generics (Generic)
import Rift.Environment (Environment (..))
import qualified Rift.Logger as Logger
import System.Exit (exitFailure)
import Text.Read (readMaybe)
import Turtle (ExitCode (..))

data Snapshot = Snapshot
  { name :: LTSVersion,
    gzcVersion :: Text,
    packageSet :: [Package]
  }
  deriving (Generic, Show)

data Package = Pkg
  { name :: Text,
    version :: Text,
    src :: PackageSource,
    component :: Maybe Text,
    maintainers :: [Text],
    broken :: Bool
  }
  deriving (Generic, Show)

data PackageSource
  = Git
      { url :: Text,
        rev :: Text,
        sha256 :: Text
      }
  | Tar
      { url :: Text,
        sha256 :: Text
      }
  deriving (Generic, Show)

data LTSVersion
  = LTS Int Int
  | Unstable
  deriving (Eq, Ord, Generic)

instance Hashable LTSVersion

instance Show LTSVersion where
  show Unstable = "unstable"
  show (LTS x y) = "lts-" <> show x <> "." <> show y

instance FromDhall LTSVersion where
  autoWith _ =
    union $
      fold
        [ Unstable <$ constructor "Unstable" unit,
          constructor "Stable" ltsField
        ]
    where
      ltsField =
        record $
          LTS
            <$> (fromIntegral <$> field "major" natural)
            <*> (fromIntegral <$> field "minor" natural)

instance ToDhall LTSVersion where
  injectWith _ =
    adapt
      >$< unionEncoder
        ( encodeConstructor "Unstable"
            >|< encodeConstructorWith "Stable" stableField
        )
    where
      stableField = recordEncoder $ id >$< encodeField "major" >*< encodeField "minor"

      adapt Unstable = Left ()
      adapt (LTS major minor) = Right (major, minor)

instance FromDhall Snapshot where
  autoWith _ =
    record $
      Snapshot
        <$> field "name" auto
        <*> field "gzc-version" auto
        <*> field "package-set" auto

instance FromDhall Package where
  autoWith _ =
    record $
      Pkg
        <$> field "name" auto
        <*> field "version" auto
        <*> field "src" auto
        <*> field "component" auto
        <*> field "maintainers" auto
        <*> field "broken" auto

instance FromDhall PackageSource where
  autoWith _ =
    union $
      fold
        [ constructor "Git" git,
          constructor "Tar" tar
        ]
    where
      git =
        record $
          Git
            <$> field "url" auto
            <*> field "rev" auto
            <*> field "sha256" auto
      tar =
        record $
          Tar
            <$> field "url" auto
            <*> field "sha256" auto

-- | Read an LTS version which is either @unstable@ or of the form @lts-<major>.<minor>@.
readLTSVersion :: Text -> Maybe LTSVersion
readLTSVersion "unstable" = Just Unstable
readLTSVersion v =
  case Text.splitOn "-" v of
    [_, ver] -> case readMaybe @Int . Text.unpack <$> Text.splitOn "." ver of
      [Just major, Just minor] -> Just $ LTS major minor
      _ -> Nothing
    _ -> Nothing

-- | Converts a dhall snapshot specification into a concrete 'Snapshot' value.
snapshotFromDhallFile :: FilePath -> Environment -> IO Snapshot
snapshotFromDhallFile dhallFile _ = handle' $ inputFile auto dhallFile
  where
    handle' act =
      catches
        act
        [ Handler handleTypeError,
          Handler handleImported,
          Handler handleExitCode
        ]

    handleAll e = do
      let str = show (e :: SomeException)
      when (not $ null str) do
        Logger.error $ "Encountered an error while parsing the package set:\n" <> Text.pack str
      exitFailure

    handleTypeError e = handle handleAll do
      let _ = e :: TypeError Src Void
      throwIO e

    handleImported (Imported ps e) = handle handleAll do
      let _ = e :: TypeError Src Void
      throwIO (Imported ps e)

    handleExitCode e = throwIO (e :: ExitCode)

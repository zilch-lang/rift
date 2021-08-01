{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wno-name-shadowing #-}

module Rift.Config.PackageSet where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)

import Rift.Environment (Environment(..))
import qualified Rift.Logger as Logger

import System.Exit (exitFailure)

import Turtle (procStrictWithErr, ExitCode(..), empty)


data Snapshot
  = Snapshot
  { name        :: Text
  , gzcVersion  :: Text
  , packageSet  :: [Package]
  }
  deriving (Show)

data Package
  = Pkg
  { name        :: Text
  , version     :: Text
  , src         :: PackageSource
  , component   :: Maybe Text
  , maintainers :: [Text]
  , broken      :: Bool
  }
  deriving (Show)

data PackageSource
  = Git
  { url         :: Text
  , rev         :: Text
  , sha256      :: Text
  }
  | Tar
  { url         :: Text
  , sha256      :: Text
  }
  deriving (Show)


instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot"
    \ v -> Snapshot <$> v .: "name"
                    <*> v .: "gzc-version"
                    <*> v .: "package-set"

instance FromJSON Package where
  parseJSON = withObject "Package"
    \ v -> Pkg <$> v .:  "name"
               <*> v .:  "version"
               <*> v .:  "src"
               <*> v .:? "component"
               <*> v .:  "maintainers"
               <*> v .:  "broken"

instance FromJSON PackageSource where
  parseJSON = withObject "Source"
    \ v -> do Git <$> v .: "url"
                  <*> v .: "rev"
                  <*> v .: "sha256"
       <|> do Tar <$> v .: "url"
                  <*> v .: "sha256"

-- | Converts a dhall snapshot specification into a concrete 'Snapshot' value.
snapshotFromDhallFile :: MonadIO m => FilePath -> Environment -> m Snapshot
snapshotFromDhallFile dhallFile Env{..} = do
  (exit, out, err) <- procStrictWithErr (Text.pack dhallToJson) [ "--compact", "--preserve-null", "--file", Text.pack dhallFile ] empty
  unless (exit == ExitSuccess) do
    Logger.error $ "'dhall-to-json' exited with a non 0 exit status.\n* Standard output:\n"
                                                                        <> Text.unlines (mappend "> " <$> Text.lines out) <> "\n* Standard error:\n"
                                                                        <> Text.unlines (mappend "> " <$> Text.lines err)
    liftIO exitFailure

  case eitherDecode' @Snapshot (ByteString.fromStrict $ Text.encodeUtf8 out) of
    Left err -> do
      Logger.error $ "Encountered an error while parsing package set:\n" <> Text.pack err
      liftIO exitFailure
    Right s  -> pure s

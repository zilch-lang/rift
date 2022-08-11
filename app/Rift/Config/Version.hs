{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Config.Version where

import Data.Hashable (Hashable)
import Data.Text (Text)
import Dhall.Marshal.Decode (Decoder (..), FromDhall (..), Natural, auto, field, natural, record)
import Dhall.Marshal.Encode (ToDhall (..), encodeField, recordEncoder, (>$<), (>*<))
import GHC.Generics (Generic)
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Text as Pretty

data PackageDependency
  = Version
      Text
      -- ^ The name of the package.
      VersionConstraint
      -- ^ The version range, in the form of, for example, @>= version@.

instance FromDhall PackageDependency where
  autoWith _ =
    record $
      Version
        <$> field "package" auto
        <*> field "version" auto

data SemVer
  = SemVersion
      Int
      -- ^ major
      Int
      -- ^ minor
      Int
      -- ^ bug fix
  deriving (Generic, Eq)

instance Hashable SemVer

instance Show SemVer where
  show (SemVersion major minor bug) = show major <> "." <> show minor <> "." <> show bug

instance Ord SemVer where
  SemVersion major1 minor1 bug1 < SemVersion major2 minor2 bug2 =
    or
      [ major1 < major2,
        major1 == major2 && minor1 < minor2,
        major1 == major2 && minor1 == minor2 && bug1 < bug2
      ]
  v1 <= v2 = or [v1 < v2, v1 == v2]

instance FromDhall SemVer where
  autoWith _ =
    record $
      SemVersion
        <$> (fromIntegral <$> field "major" natural)
        <*> (fromIntegral <$> field "minor" natural)
        <*> (fromIntegral <$> field "bug" natural)

instance ToDhall SemVer where
  injectWith _ =
    recordEncoder $
      adjust
        >$< encodeField "major"
        >*< encodeField "minor"
        >*< encodeField "bug"
    where
      adjust :: SemVer -> (Natural, (Natural, Natural))
      adjust (SemVersion major minor bug) = (fromIntegral major, (fromIntegral minor, fromIntegral bug))

type VersionConstraint = (Text, SemVer -> Bool)

instance {-# OVERLAPPING #-} FromDhall VersionConstraint where
  autoWith _ =
    let decoder = auto :: Decoder (SemVer -> Bool)
     in Decoder
          { extract = \e -> (pretty' e,) <$> extract decoder e,
            expected = expected decoder
          }
    where
      pretty' = Pretty.renderStrict . Pretty.layoutCompact . Pretty.group . Pretty.pretty

trueConstraint :: VersionConstraint
trueConstraint = ("λ(v : Version.Type) → True", const True)

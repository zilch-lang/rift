{-# LANGUAGE OverloadedStrings #-}

module Rift.Config.Configuration where

import Dhall.Marshal.Decode (FromDhall (..), auto, field, record)
import Rift.Config.Package (ExtraPackage)
import Rift.Config.PackageSet (LTSVersion)

data Configuration
  = Configuration
      LTSVersion
      -- ^ The version of the LTS used.
      [ExtraPackage]
      -- ^ Extra dependencies which are not in the package set.
  deriving (Show)

instance FromDhall Configuration where
  autoWith _ =
    record $
      Configuration
        <$> field "lts" auto
        <*> field "extra-deps" auto

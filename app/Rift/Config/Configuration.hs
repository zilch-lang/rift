{-# LANGUAGE OverloadedStrings #-}

module Rift.Config.Configuration where

import Dhall.Marshal.Decode (FromDhall (..), auto, field, record)
import Rift.Config.PackageSet (LTSVersion)
import Rift.Config.Source (Source)

data Configuration
  = Configuration
      LTSVersion
      -- ^ The version of the LTS used.
      [Source]
      -- ^ Extra dependencies which are not in the package set.
  deriving (Show)

instance FromDhall Configuration where
  autoWith _ =
    record $
      Configuration
        <$> field "lts" auto
        <*> field "extra-deps" auto

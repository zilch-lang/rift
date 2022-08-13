{-# LANGUAGE OverloadedStrings #-}

module Rift.Config.Configuration where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Dhall.Marshal.Decode (FromDhall (..), auto, field, record)
import Rift.Config.Package (ExtraPackage)
import Rift.Config.PackageSet (LTSVersion (Unstable))

data Configuration
  = Configuration
      LTSVersion
      -- ^ The version of the LTS used.
      [ExtraPackage]
      -- ^ Extra dependencies which are not in the package set.
      Bool
      -- ^ Whether to use the GZC compiler installed on the system or the one bundled with the LTS.
      (Maybe Text)
      -- ^ An optional path to a specific GZC compiler to use.
  deriving (Show)

instance FromDhall Configuration where
  autoWith _ =
    record $
      Configuration
        <$> field "lts" auto
        <*> field "extra-deps" auto
        <*> field "system-gzc" auto
        <*> field "gzc-path" auto

empty :: Configuration
empty = Configuration Unstable [] False Nothing

merge :: Maybe Configuration -> Maybe Configuration -> Configuration
merge Nothing Nothing = empty
merge (Just c1) Nothing = c1
merge Nothing (Just c2) = c2
merge (Just (Configuration lts1 extra1 sysgzc1 gzcpath1)) (Just (Configuration _ extra2 sysgzc2 gzcpath2)) =
  Configuration
    lts1
    (extra1 <> extra2)
    (sysgzc1 || sysgzc2)
    (gzcpath1 <|> gzcpath2)

useSystemGZC :: Configuration -> Bool
useSystemGZC (Configuration _ _ sys _) = sys

systemGZCPath :: Configuration -> Maybe Text
systemGZCPath (Configuration _ _ _ path) = path

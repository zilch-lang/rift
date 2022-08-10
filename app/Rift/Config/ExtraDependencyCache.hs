{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Rift.Config.ExtraDependencyCache where

import Data.Map (Map)
import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MultiMap
import Data.Text (Text)
import Dhall.Marshal.Decode (FromDhall (..), auto, field, record)
import Dhall.Marshal.Encode (ToDhall (..), encodeField, recordEncoder, (>$<), (>*<))
import Rift.Config.Source (Source)
import Rift.Config.Version (SemVer)

instance (Ord k, FromDhall k, FromDhall v) => FromDhall (MultiMap k v) where
  autoWith cfg = MultiMap.fromMap <$> autoWith cfg

instance (Ord k, ToDhall k, ToDhall v) => ToDhall (MultiMap k v) where
  injectWith cfg = MultiMap.toMap >$< injectWith cfg

data ExtraCache
  = ExtraCache
      (MultiMap Text SemVer)
      (Map (Text, SemVer) (FilePath, Source))

instance FromDhall ExtraCache where
  autoWith _ =
    record $
      ExtraCache
        <$> field "versions" auto
        <*> field "paths" auto

instance ToDhall ExtraCache where
  injectWith _ =
    recordEncoder $
      adapt
        >$< encodeField "versions"
        >*< encodeField "paths"
    where
      adapt (ExtraCache versions paths) = (versions, paths)

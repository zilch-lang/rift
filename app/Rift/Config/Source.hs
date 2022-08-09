{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Config.Source where

import Data.Foldable (fold)
import Data.Text (Text)
import Dhall (Generic)
import Dhall.Marshal.Decode (FromDhall (..), auto, constructor, field, record, union)
import Dhall.Marshal.Encode (ToDhall (..), encodeConstructorWith, encodeField, recordEncoder, unionEncoder, (>$<), (>*<), (>|<))

-- unfortunately we cannot write a decoder for 'Dhall.Core.Import' because the decoder takes a @'Dhall.Core.Expr' s Void@.
data Location
  = Environment
      Text
  | Local
      Text
  | Missing
  | Remote
      Text
  deriving (Generic, Show, FromDhall, ToDhall, Eq)

data Source
  = Git
      Location
      Text
  | Tar
      Location
      Text
  | TarGz
      Location
      Text
  | Zip
      Location
      Text
  deriving (Show, Eq)

instance FromDhall Source where
  autoWith _ =
    union $
      fold
        [ constructor "Git" git,
          constructor "Tar" tar,
          constructor "TarGz" targz,
          constructor "Zip" zip
        ]
    where
      git =
        record $
          Git
            <$> field "url" auto
            <*> field "rev" auto
      tar =
        record $
          Tar
            <$> field "url" auto
            <*> field "sha256" auto
      targz =
        record $
          TarGz
            <$> field "url" auto
            <*> field "sha256" auto
      zip =
        record $
          Zip
            <$> field "url" auto
            <*> field "sha256" auto

instance ToDhall Source where
  injectWith _ =
    adapt
      >$< unionEncoder
        ( encodeConstructorWith "Git" git
            >|< encodeConstructorWith "Tar" tar
            >|< encodeConstructorWith "TarGz" targz
            >|< encodeConstructorWith "Zip" zip
        )
    where
      adapt (Git url rev) = Left (url, rev)
      adapt (Tar url sha) = Right (Left (url, sha))
      adapt (TarGz url sha) = Right (Right (Left (url, sha)))
      adapt (Zip url sha) = Right (Right (Right (url, sha)))

      git = recordEncoder $ encodeField "url" >*< encodeField "rev"
      tar = recordEncoder $ encodeField "url" >*< encodeField "sha256"
      targz = recordEncoder $ encodeField "url" >*< encodeField "sha256"
      zip = recordEncoder $ encodeField "url" >*< encodeField "sha256"

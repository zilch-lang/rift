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

prettyLocation :: Location -> Text
prettyLocation (Environment src) = "env:" <> src
prettyLocation (Local file) = file
prettyLocation (Remote url) = url
prettyLocation Missing = "missing"

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
  | Directory
      Location
  deriving (Show, Eq)

prettySource :: Source -> Text
prettySource (Git loc rev) = "Git { url = " <> prettyLocation loc <> " as Location, rev = \"" <> rev <> "\" }"
prettySource (Tar loc sha256) = "Tar { url = " <> prettyLocation loc <> " as Location, sha256 = \"" <> sha256 <> "\" }"
prettySource (TarGz loc sha256) = "TarGz { url = " <> prettyLocation loc <> " as Location, sha256 = \"" <> sha256 <> "\" }"
prettySource (Zip loc sha256) = "Zip { url = " <> prettyLocation loc <> " as Location, sha256 = \"" <> sha256 <> "\" }"
prettySource (Directory loc) = "Directory { path = " <> prettyLocation loc <> " as Location }"

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

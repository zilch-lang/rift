module Rift.Commands.Impl.Utils.Paths where

import System.FilePath ((<.>))

-- | The file containing all components inside the project.
projectDhall :: FilePath
projectDhall = "project" <.> "dhall"

-- | The file containing information such as the LTS used to compile the project, extra dependencies not present in the official package set, etc.
riftDhall :: FilePath
riftDhall = "rift" <.> "dhall"

-- | A simple file to setup the environment easily.
envDhall :: FilePath
envDhall = "env" <.> "dhall"

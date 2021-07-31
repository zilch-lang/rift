module Rift.Environment.Def (Environment, Environment(..)) where

data Environment
  = Env
  { riftHome    :: FilePath
  , pkgsHome    :: FilePath
  , dhallToJson :: FilePath
  }

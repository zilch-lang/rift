module Rift.Environment.Def (Environment (..)) where

-- | A slice of the user environment, memorized here to query only once.
data Environment = Env
  { -- | Equivalent to the @$RIFT_HOME@ environment variable
    riftHome :: FilePath,
    -- | Path to @$RIFT_HOME/pkgs@
    pkgsHome :: FilePath,
    -- | Path to tge @git@ executable
    git :: FilePath
  }

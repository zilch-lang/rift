module Rift.Environment.Def (Environment (..)) where

-- | A slice of the user environment, memorized here to query only once.
data Environment = Env
  { -- | Equivalent to the @$RIFT_HOME@ environment variable
    riftHome :: FilePath,
    -- | Path to @$RIFT_CACHE@
    riftCache :: FilePath,
    -- | Path to @$RIFT_CONFIG@
    riftConfig :: FilePath,
    -- | Path to tge @git@ executable
    git :: FilePath
  }

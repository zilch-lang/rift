module Rift.Environment.Def (Environment(..)) where


-- | A slice of the user environment, memorized here to query only once.
data Environment
  = Env
  { riftHome    :: FilePath  -- ^ Equivalent to the @$RIFT_HOME@ environment variable
  , pkgsHome    :: FilePath  -- ^ Path to @$RIFT_HOME/pkgs@
  , dhallToJson :: FilePath  -- ^ Path to the @dhall-to-json@ executable
  , git         :: FilePath  -- ^ Path to tge @git@ executable
  }

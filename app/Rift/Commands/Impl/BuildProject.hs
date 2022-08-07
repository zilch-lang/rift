module Rift.Commands.Impl.BuildProject where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Rift.Environment (Environment)

buildProjectCommand :: (MonadIO m) => Bool -> Integer -> Bool -> [Text] -> Environment -> m ()
buildProjectCommand dryRun nbCores dirtyFiles components env = do
  pure ()

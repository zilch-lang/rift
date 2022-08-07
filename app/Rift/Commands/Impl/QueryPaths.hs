module Rift.Commands.Impl.QueryPaths where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Rift.Environment (Environment)

queryAndPrintPaths :: (MonadIO m) => Text -> Environment -> m ()
queryAndPrintPaths key env = error "TODO: query paths"

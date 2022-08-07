{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Rift.Commands.Impl.PrintPaths where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (sort)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Rift.Environment (Environment (..))
import qualified System.Envy as E

printPaths :: (MonadIO m) => Environment -> m ()
printPaths Env {..} = do
  riftCfg <-
    liftIO $
      E.runEnv (E.envMaybe @FilePath "RIFT_CFG") >>= \case
        Left _ -> pure "undefined"
        Right Nothing -> pure "undefined"
        Right (Just path) -> pure path

  liftIO do
    let paths =
          sort
            [ "rift-home: " <> Text.pack riftHome,
              "pkgs-home: " <> Text.pack pkgsHome,
              "rift-cache: " <> Text.pack riftCache,
              "rift-config: " <> Text.pack riftCfg
            ]
    mapM_ Text.putStrLn paths
  pure ()

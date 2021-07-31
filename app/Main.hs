{-# LANGUAGE BlockArguments #-}

module Main where

import Environment
import Flags

main :: IO ()
main = do
  cmd <- parseCLI

  dhallJsonExe <- setupEnv case cmd of
    UpdateSet -> False
    _         -> True

  pure ()

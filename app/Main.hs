{-# LANGUAGE BlockArguments #-}

module Main where

import Command (executeCommand, Command(..))
import Environment
import Flags

main :: IO ()
main = do
  cmd <- parseCLI

  env <- setupEnv case cmd of
    Update -> False
    _      -> True

  executeCommand cmd env

  pure ()

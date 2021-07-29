module Main where

import Environment
import Flags

main :: IO ()
main = do
  cmd <- parseCLI

  setupEnv

  pure ()

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rift.Commands.Impl.Utils.DhallHash where

import Control.Exception (Handler (..), SomeException, catches, handle, throwIO)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Dhall.Core (alphaNormalize, normalize)
import qualified Dhall.Core
import Dhall.Import (Imported (Imported), SemanticCacheMode (IgnoreSemanticCache), hashExpressionToCode, loadRelativeTo)
import Dhall.Src (Src)
import Dhall.TypeCheck (TypeError, typeOf)
import Dhall.Util (Censor (NoCensor), Input (InputFile), getExpression)
import qualified Rift.Logger as Logger
import System.Exit (ExitCode, exitFailure)
import System.FilePath (takeDirectory)

-- | Hash the Dhall file given.
dhallHash :: FilePath -> IO Text
dhallHash file = handle' do
  expr <- getExpression NoCensor (InputFile file)
  resolvedExpr <- loadRelativeTo (takeDirectory file) IgnoreSemanticCache expr

  _ <- Dhall.Core.throws (typeOf resolvedExpr)
  let normalizedExpr = alphaNormalize (normalize resolvedExpr)

  pure (hashExpressionToCode normalizedExpr)
  where
    handle' act =
      catches
        act
        [ Handler handleTypeError,
          Handler handleImported,
          Handler handleExitCode
        ]

    handleAll e = do
      let str = show (e :: SomeException)
      when (not $ null str) do
        Logger.error $ "Encountered an error while parsing the package set:\n" <> Text.pack str
      exitFailure

    handleTypeError e = handle handleAll do
      let _ = e :: TypeError Src Void
      throwIO e

    handleImported (Imported ps e) = handle handleAll do
      let _ = e :: TypeError Src Void
      throwIO (Imported ps e)

    handleExitCode e = throwIO (e :: ExitCode)

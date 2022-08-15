{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.FetchPackage where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Impl.Utils.Download (fetchPackageTo', resolvePackage)
import Rift.Commands.Impl.Utils.Paths (ltsPath)
import Rift.Config.Package (Package (version))
import Rift.Config.PackageSet (LTSVersion (..), readLTSVersion)
import Rift.Config.Version (trueConstraint)
import Rift.Config.Version.Parser (parseVersionConstraint)
import Rift.Environment (Environment (..))
import qualified System.Console.ANSI as ANSI
import System.FilePath ((</>))
import System.IO (stdout)

fetchPackageCommand :: (?logLevel :: Int, MonadIO m, MonadHttp m, MonadMask m) => Text -> Maybe Text -> Maybe Text -> Bool -> Environment -> m ()
fetchPackageCommand name versionConstraint ltsName force env = do
  let lts = fromMaybe Unstable (ltsName >>= readLTSVersion)
  constr <- fromMaybe trueConstraint <$> traverse parseVersionConstraint versionConstraint
  (pkgPath, pkg, isExtra) <- resolvePackage name lts constr [] env
  --             ^^^^^^^ this /should/ always be 'False' here

  ltsDir <- fromJust <$> ltsPath (riftCache env) lts

  void $ fetchPackageTo' lts (ltsDir </> "sources") (riftCache env </> "extra-deps") force True isExtra env pkgPath pkg

  liftIO do
    Text.hPutStr stdout "Package "
    ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
    Text.hPutStr stdout name
    ANSI.hSetSGR stdout [ANSI.Reset]
    Text.hPutStr stdout " (version "
    ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
    Text.hPutStr stdout $ Text.pack $ show $ version pkg
    ANSI.hSetSGR stdout [ANSI.Reset]
    Text.hPutStr stdout ") fetched into the cache of the LTS "
    ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
    Text.hPutStr stdout $ Text.pack $ show lts
    ANSI.hSetSGR stdout [ANSI.Reset]
    Text.hPutStrLn stdout "!"

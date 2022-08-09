{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.BuildProject where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Dhall (auto, inputFile)
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Impl.Utils.Download (downloadAndExtract)
import Rift.Commands.Impl.Utils.ExtraDependencyCacheManager (insertExtraDependency)
import Rift.Commands.Impl.Utils.Paths (projectDhall, riftDhall, sourcePath)
import Rift.Config.Configuration (Configuration (..))
import Rift.Config.Package (ExtraPackage (..))
import Rift.Config.PackageSet (Snapshot (..), snapshotFromDhallFile)
import Rift.Config.Project (ComponentType (..), ProjectType, nameOf)
import Rift.Config.Source (Source)
import Rift.Environment (Environment, riftCache)
import qualified Rift.Logger as Logger
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))

-- | Building the project happens in multiple steps:
--
--   1. Parse the @package.dhall@ file to retrieve all components and (extra-)dependencies
--
--   2. Fetch every dependency (with its version) from the package set inside the global cache in @env:RIFT_HOME@
--
--      We may want to put them inside @~\/.cache\/rift@ instead, where each entry is organized as:
--
--      > ~/.cache/rift/
--      > └── <lts name>.<hash of 'lts/packages/set.dhall'>/
--      >     ├── lts
--      >     │   ├── lib
--      >     │   ├── packages
--      >     │   │   └── set.dhall
--      >     │   └── utils
--      >     └── sources
--      >         └── <component name>.<component version>/
--      >             ├── project.dhall
--      >             └── ...
--
--   3. Create a dependency tree, taking in account both and extra- and intra-dependencies
--
--   4. Topsort the dependency tree to prevent any cycle in dependency resolution
--
--   5. Try building every dependency starting from the beginning of the topsort
--      /Note:/ Our topsort may contain concurrent branches: build those packages concurrently (as long as there are processors remaining)
--
--   5.1. If a dependency is a library, simply execute the command @gzc FLAGS MODULES -I DIRS -ddump-dir=$PWD/.rift/build --keep-zco@ inside its cache directory where:
--
--          * @MODULES@ is the list of modules found in the source directories (the compiler will order them as wanted with an additional topsort)
--          * @DIRS@ is a comma-separated list containing the source directories of the component, as well as the directories containing code in every dependency
--
--   5.2. If a dependency is an executable, the behavior is not yet fixed (we may want to throw an error here)
--
--   6. I don't know
buildProjectCommand :: (MonadIO m, MonadHttp m, MonadMask m) => Bool -> Integer -> Bool -> [Text] -> Environment -> m ()
buildProjectCommand dryRun nbCores dirtyFiles componentsToBuild env = do
  !components <- liftIO $ inputFile auto projectDhall
  Configuration lts extraDeps <- liftIO $ inputFile auto riftDhall

  let ltsTag = show lts
      ltsHashFile = riftCache env </> "hashes" </> ltsTag <.> "hash"
  ltsHash <- liftIO $ Text.readFile ltsHashFile
  let ltsDir = riftCache env </> (ltsTag <> "." <> Text.unpack ltsHash)

  componentsToBuild <- case componentsToBuild of
    [] -> do
      Logger.debug "No component specified. Building all local components."
      pure components
    cs -> getComponentsByName components (nub cs)

  snapshot <- liftIO $ snapshotFromDhallFile (ltsDir </> "lts" </> "packages" </> "set.dhall") env
  extraDeps' <- fetchExtraDependencies env extraDeps
  (resolvedDeps1, unresolvedDeps) <- gatherDependencies snapshot componentsToBuild
  (resolvedDeps2, unresolvedDeps) <- checkUnresolvedDependencies extraDeps' (nameOf <$> components) unresolvedDeps

  pure ()

getComponentsByName :: (MonadIO m) => [ComponentType] -> [Text] -> m [ComponentType]
getComponentsByName _ [] = pure []
getComponentsByName components (c : cs) = case findElem (\(ComponentType name _ _ _ _ _) -> name == c) components of
  Nothing -> do
    Logger.error $ "Component named '" <> c <> "' not found in local project."
    liftIO exitFailure
  Just (c1, others) -> (c1 :) <$> getComponentsByName others cs

gatherDependencies :: (MonadIO m) => Snapshot -> [ComponentType] -> m ([Text], [Text])
gatherDependencies _ [] = pure ([], [])
gatherDependencies snapshot (ComponentType _ _ deps _ _ _ : cs) = pure ([], [])

fetchExtraDependencies :: (MonadIO m, MonadHttp m, MonadMask m) => Environment -> [ExtraPackage] -> m (Map FilePath ProjectType)
fetchExtraDependencies _ [] = pure mempty
fetchExtraDependencies env (ExtraPkg name version dep _ : deps) = do
  done <- fetchExtraDependencies env deps

  let srcPath = sourcePath (riftCache env </> "extra-deps") dep
  (path, project, _) <- downloadAndExtract srcPath dep True env

  insertExtraDependency name version path dep env

  pure (Map.insert path project done)

checkUnresolvedDependencies :: (MonadIO m) => Map FilePath ProjectType -> [Text] -> [Text] -> m ([Text], [Text])
checkUnresolvedDependencies _ _ [] = pure ([], [])

------------------------------

findElem :: (a -> Bool) -> [a] -> Maybe (a, [a])
findElem p = find' id
  where
    find' _ [] = Nothing
    find' prefix (x : xs)
      | p x = Just (x, prefix xs)
      | otherwise = find' (prefix . (x :)) xs

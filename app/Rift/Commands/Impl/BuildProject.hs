{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.BuildProject where

import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import qualified Algebra.Graph.AdjacencyMap as Cyclic
import Control.Exception (throwIO)
import Control.Monad (forM, void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Dhall (auto, inputFile)
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Impl.Utils.Config (readPackageDhall)
import Rift.Commands.Impl.Utils.Download (fetchPackageTo, resolvePackage)
import Rift.Commands.Impl.Utils.Paths (ltsPath, riftDhall)
import Rift.Config.Configuration (Configuration (..))
import Rift.Config.Package (Package (..))
import Rift.Config.PackageSet (LTSVersion, Snapshot (..), snapshotFromDhallFile)
import Rift.Config.Project (ComponentType (..))
import Rift.Config.Source (Location (Local), Source (Directory))
import Rift.Config.Version (PackageDependency (..))
import Rift.Environment (Environment, riftCache)
import Rift.Internal.Exceptions (RiftException (..))
import qualified Rift.Logger as Logger
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

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
  !components <- readPackageDhall "."
  Configuration lts extraDeps <- liftIO $ inputFile auto riftDhall

  let ~ltsErr = liftIO $ throwIO (LTSNotFound lts)
  ltsDir <- maybe ltsErr pure =<< ltsPath (riftCache env) lts

  componentsToBuild <- case componentsToBuild of
    [] -> do
      Logger.debug "No component specified. Building all local components."
      pure components
    cs -> getComponentsByName (Map.restrictKeys components (Set.fromList cs)) (nub cs)

  snapshot <- liftIO $ snapshotFromDhallFile (ltsDir </> "lts" </> "packages" </> "set.dhall") env

  -- TODO: fetch and build all extra dependencies, we may need them
  -- but beware of dependency cycles!

  void $ flip Map.traverseWithKey componentsToBuild (buildComponent lts ltsDir snapshot dryRun dirtyFiles env)

  pure ()

getComponentsByName :: (MonadIO m) => Map Text a -> [Text] -> m (Map Text a)
getComponentsByName _ [] = pure mempty
getComponentsByName components (c : cs) = case Map.lookup c components of
  Nothing -> liftIO $ throwIO (NoSuchComponent c)
  Just c1 ->
    let others = Map.delete c components
     in Map.insert c c1 <$> getComponentsByName others cs

buildComponent :: (MonadIO m, MonadHttp m, MonadMask m) => LTSVersion -> FilePath -> Snapshot -> Bool -> Bool -> Environment -> Text -> ComponentType -> m ()
buildComponent lts ltsDir snapshot dryRun dirtyFiles env name (ComponentType ver deps sources kind flags) = do
  cwd <- liftIO getCurrentDirectory
  let thisPkg = Pkg name ver (Directory $ Local $ Text.pack cwd) [] False False

  -- get every dependency and fetch them in the cache
  -- beware of cycles when constructing the dependency graph
  !graph <-
    fromJust . Acyclic.toAcyclic . Cyclic.connects <$> forM deps \(Version name versionConstraint) -> do
      depPkg <- resolvePackage name lts versionConstraint False [thisPkg] env
      fetchPackageTo (Cyclic.edge thisPkg depPkg) lts ltsDir (riftCache env </> "extra-deps") False False env depPkg

  -- now that we have the dependency graph, topsort it
  -- NOTE: if a /dependency/ is an executable, abort
  let sorted = reverse $ Acyclic.topSort graph

  liftIO $ print sorted

  -- the topsort gives us the order in which to build dependencies (our package should come last)
  -- however its order may need to be reversed

  --
  pure ()

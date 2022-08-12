{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.BuildProject where

import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import qualified Algebra.Graph.AdjacencyMap as Cyclic
import Control.Exception (throwIO)
import Control.Monad (forM, void, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first, second)
import Data.Bool (bool)
import Data.Foldable (foldrM)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (nub, uncons)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Dhall (auto, inputFile)
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Impl.Utils.Config (readPackageDhall)
import Rift.Commands.Impl.Utils.Download (fetchPackageTo, resolvePackage)
import Rift.Commands.Impl.Utils.Paths (dotRift, ltsPath, riftDhall)
import Rift.Config.Configuration (Configuration (..))
import Rift.Config.Package (Package (..))
import Rift.Config.PackageSet (LTSVersion)
import Rift.Config.Project (ComponentType (..))
import Rift.Config.Source (Location (Local), Source (Directory))
import Rift.Config.Version (PackageDependency (..))
import Rift.Environment (Environment, riftCache)
import Rift.Internal.Exceptions (RiftException (..))
import qualified Rift.Logger as Logger
import qualified System.Console.ANSI as ANSI
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (stdout)

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
--   5.1. If a dependency is a library, simply execute the command @gzc FLAGS MODULES -I DIRS -ddump-dir=$PWD/.rift/build --keep-zco --keep-zci@ inside its cache directory where:
--
--          * @MODULES@ is the list of modules found in the source directories (the compiler will order them as wanted with an additional topsort)
--          * @DIRS@ is a comma-separated list containing the source directories of the component, as well as the directories containing code in every dependency
--
--   5.2. If a dependency is an executable, the behavior is not yet fixed (we may want to throw an error here)
--
--   6. I don't know
buildProjectCommand :: (?logLevel :: Int, MonadIO m, MonadHttp m, MonadMask m) => Bool -> Integer -> Bool -> [Text] -> Environment -> m ()
buildProjectCommand dryRun nbCores dirtyFiles componentsToBuild env = do
  !components <- readPackageDhall "."
  Configuration lts extraDeps <- liftIO $ inputFile auto riftDhall

  let ~ltsErr = liftIO $ throwIO (LTSNotFound lts)
  ltsDir <- maybe ltsErr pure =<< ltsPath (riftCache env) lts

  -- TODO: fetch the GZC compiler from the LTS information (see 'ltsDir </> "lts" </> "packages" </> "set" <.> "dhall"') into the cache
  -- and keep the path to it in memory to compile packages later

  componentsToBuild <- case componentsToBuild of
    [] -> do
      Logger.debug "No component specified. Building all local components."
      pure components
    cs -> getComponentsByName (Map.restrictKeys components (Set.fromList cs)) (nub cs)

  -- snapshot <- liftIO $ snapshotFromDhallFile (ltsDir </> "lts" </> "packages" </> "set.dhall") env

  -- TODO: fetch and build all extra dependencies, we may need them
  -- but beware of dependency cycles!

  -- FIXME: we will want to also allow dependencies across local components
  void $ flip Map.traverseWithKey componentsToBuild (buildComponent lts ltsDir dryRun dirtyFiles env)

  pure ()

getComponentsByName :: (MonadIO m) => Map Text a -> [Text] -> m (Map Text a)
getComponentsByName _ [] = pure mempty
getComponentsByName components (c : cs) = case Map.lookup c components of
  Nothing -> liftIO $ throwIO (NoSuchComponent c)
  Just c1 ->
    let others = Map.delete c components
     in Map.insert c c1 <$> getComponentsByName others cs

buildComponent :: (?logLevel :: Int, MonadIO m, MonadHttp m, MonadMask m) => LTSVersion -> FilePath -> Bool -> Bool -> Environment -> Text -> ComponentType -> m ()
buildComponent lts ltsDir dryRun dirtyFiles env name (ComponentType ver deps sources kind flags) = do
  cwd <- liftIO getCurrentDirectory
  let thisPkg' = Pkg name ver (Directory $ Local $ Text.pack cwd) [] False False

  -- get every dependency and fetch them in the cache
  -- beware of cycles when constructing the dependency graph
  !graph <-
    Cyclic.overlays <$> forM deps \(Version name versionConstraint) -> do
      (depPkgPath, depPkg) <- resolvePackage name lts versionConstraint [thisPkg'] env
      fetchPackageTo (Cyclic.edge (cwd, thisPkg') (depPkgPath, depPkg)) lts (ltsDir </> "sources") (riftCache env </> "extra-deps") False False env depPkgPath depPkg

  -- remove all the packages which do not need to be rebuilt
  -- one way to do this is to store the timestamp of the last modified file in the project in the @<project dir>/.rift@ directory
  --   and check that no file has been modified afterwards
  (graph, rebuild) <- first (fromJust . Acyclic.toAcyclic) <$> removeUnmodifiedLibs graph (cwd, thisPkg')

  -- now that we have the dependency graph, topsort it
  -- NOTE: if a /dependency/ is an executable, abort
  let Just (thisPkg, sorted) =
        if Acyclic.vertexCount graph == 0
          then Just ((cwd, thisPkg'), [])
          else fmap (second reverse) (uncons $ Acyclic.topSort graph)

  -- the topsort gives us the order in which to build dependencies (our package should come last)
  -- however its order may need to be reversed
  nbOutOf :: IORef Integer <- liftIO $ newIORef 1
  let total = length sorted + 1
      tot = show total
  includeDirs <- forM sorted \(path, pkg) -> do
    buildPackage' nbOutOf tot [] False path pkg

    liftIO $ modifyIORef nbOutOf (+ 1)
    pure $ dotRift path

  -- now build our package
  --
  -- the include directories returned will allow us to point to various useful data:
  -- - 'dir/interfaces' contains '.zci' files describing interfaces of modules
  -- - 'dir/objects' contains '.zco' objects containing compiled object files almost ready to be linked together

  let rebuildCurrent = rebuild || dirtyFiles
  when rebuildCurrent do
    uncurry (buildPackage' nbOutOf tot includeDirs True) thisPkg

  pure ()
  where
    buildPackage' nbOutOf tot includeDirs isThisPackage path pkg@(Pkg name _ _ _ _ _) = do
      liftIO do
        nbOutOf' <- readIORef nbOutOf

        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
        Text.hPutStr stdout $ "[" <> pad ' ' (length tot) (Text.pack $ show nbOutOf') <> " of " <> Text.pack tot <> "]"
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStr stdout $ " Building package "
        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
        Text.hPutStr stdout name
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStrLn stdout $ "..."

      buildPackage path pkg dryRun includeDirs isThisPackage

    pad prefixChar max txt = Text.replicate (max - Text.length txt) (Text.singleton prefixChar) <> txt

    removeUnmodifiedLibs :: (MonadIO m) => Cyclic.AdjacencyMap (FilePath, Package) -> (FilePath, Package) -> m (Cyclic.AdjacencyMap (FilePath, Package), Bool)
    removeUnmodifiedLibs graph root = do
      let check' pkg (graph, mods) = second (: mods) <$> removeUnmodifiedLibs graph pkg
      (graph, modifications) <- foldrM check' (graph, []) (Set.toList $ Cyclic.postSet root graph)

      selfModified <- checkModified root
      let notForRemoval = case modifications of
            [] ->
              -- no children so only check if package was modified
              selfModified
            _ ->
              -- if any child has been modified, then we need to be recompiled too
              or (selfModified : modifications)

      pure $
        (,notForRemoval)
          if notForRemoval
            then graph
            else Cyclic.removeVertex root graph

    checkModified (path, pkg) = pure True -- TODO

buildPackage :: (MonadIO m) => FilePath -> Package -> Bool -> [FilePath] -> Bool -> m ()
buildPackage path pkg dryRun includeDirs isCurrentProject = do
  -- when considering components, assume that:
  -- - all the dependencies were already built already, because of our topsort (although this is unreliable)
  --   ideally we'd call @buildProjectCommand@ back with a different source directory

  undefined

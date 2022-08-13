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
import Control.Monad (forM, forM_, void, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first, second)
import Data.Foldable (fold, foldrM)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (intersperse, nub, uncons)
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
import Rift.Commands.Impl.Utils.Directory (listDirectoryRecursive)
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
import System.FilePath (dropExtension, makeRelative, splitDirectories, takeExtension, (</>))
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

  -- TODO: fetch and build all extra dependencies, we may need them
  -- but beware of dependency cycles!

  cwd <- liftIO getCurrentDirectory
  let additionalPkgs = Map.elems $
        flip Map.mapWithKey components \name (ComponentType ver _ _ _ _) -> (cwd, Pkg name ver (Directory $ Local $ Text.pack cwd) [] False False)

  void $ flip Map.traverseWithKey componentsToBuild (buildComponent lts ltsDir dryRun dirtyFiles additionalPkgs env)

  pure ()

getComponentsByName :: (MonadIO m) => Map Text a -> [Text] -> m (Map Text a)
getComponentsByName _ [] = pure mempty
getComponentsByName components (c : cs) = case Map.lookup c components of
  Nothing -> liftIO $ throwIO (NoSuchComponent c)
  Just c1 ->
    let others = Map.delete c components
     in Map.insert c c1 <$> getComponentsByName others cs

buildComponent :: (?logLevel :: Int, MonadIO m, MonadHttp m, MonadMask m) => LTSVersion -> FilePath -> Bool -> Bool -> [(FilePath, Package)] -> Environment -> Text -> ComponentType -> m ()
buildComponent lts ltsDir dryRun dirtyFiles additional env name (ComponentType ver deps _ _ _) = do
  cwd <- liftIO getCurrentDirectory
  let thisPkg' = Pkg name ver (Directory $ Local $ Text.pack cwd) [] False False

  -- get every dependency and fetch them in the cache
  -- beware of cycles when constructing the dependency graph
  !graph <-
    foldrM
      ( \(Version name versionConstraint) graph -> do
          (depPkgPath, depPkg) <- resolvePackage name lts versionConstraint (snd <$> additional) env
          fetchPackageTo (graph `Cyclic.overlay` Cyclic.edge (cwd, thisPkg') (depPkgPath, depPkg)) lts (ltsDir </> "sources") (riftCache env </> "extra-deps") False False env depPkgPath depPkg
      )
      Cyclic.empty
      deps

  -- remove all the packages which do not need to be rebuilt
  -- one way to do this is to store the timestamp of the last modified file in the project in the @<project dir>/.rift@ directory
  --   and check that no file has been modified afterwards
  (graph, rebuild) <- first (fromJust . Acyclic.toAcyclic) <$> removeUnmodifiedLibs graph (cwd, thisPkg')

  -- now that we have the dependency graph, topsort it
  let Just (thisPkg, sorted) =
        if Acyclic.vertexCount graph == 0
          then Just ((cwd, thisPkg'), [])
          else fmap (second reverse) (uncons $ Acyclic.topSort graph)

  -- the topsort gives us the order in which to build dependencies (our package should come last)
  -- however its order may need to be reversed
  nbOutOf :: IORef Integer <- liftIO $ newIORef 1
  let total = length sorted + 1
      tot = show total
  forM_ sorted \(path, pkg) -> do
    buildPackage' nbOutOf tot False path pkg

    liftIO $ modifyIORef nbOutOf (+ 1)

  -- now build our package
  --
  -- the include directories returned will allow us to point to various useful data:
  -- - 'dir/interfaces' contains '.zci' files describing interfaces of modules
  -- - 'dir/objects' contains '.zco' objects containing compiled object files almost ready to be linked together

  let rebuildCurrent = rebuild || dirtyFiles
  when rebuildCurrent do
    uncurry (buildPackage' nbOutOf tot True) thisPkg

  pure ()
  where
    buildPackage' nbOutOf tot isThisPackage path pkg@(Pkg name _ _ _ _ _) = do
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

      buildPackage path pkg dryRun isThisPackage lts env

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

buildPackage :: (MonadIO m, ?logLevel :: Int, MonadHttp m, MonadMask m) => FilePath -> Package -> Bool -> Bool -> LTSVersion -> Environment -> m ()
buildPackage path pkg dryRun isCurrentProject lts env = do
  -- when considering components, assume that:
  -- - all the dependencies were already built, because of our topsort (although this is unreliable)

  project <- readPackageDhall path
  configuration <- liftIO (inputFile auto (path </> riftDhall) :: IO Configuration)
  let ComponentType _ deps srcDirs kind gzcFlags = project Map.! name pkg

  let pkgsHere = Map.elems $ flip Map.mapWithKey project \name (ComponentType ver _ _ _ _) -> Pkg name ver (Directory $ Local $ Text.pack path) [] False False
  includeDirs <- fmap fst <$> traverse (\(Version name constraint) -> resolvePackage name lts constraint pkgsHere env) deps

  let include = ((path </>) . Text.unpack <$> srcDirs) <> includeDirs
      srcDirs' = (path </>) . Text.unpack <$> srcDirs
  files <-
    liftIO $
      mconcat <$> forM srcDirs' \dir -> do
        children <- listDirectoryRecursive dir
        pure $ makeRelative dir <$> children

  let files' = filter ((== ".zc") . takeExtension) files
      modules = fold . intersperse "::" . splitDirectories . dropExtension <$> files'

  Logger.debug $ "Building modules " <> Text.pack (show $ Text.pack <$> modules) <> " of package " <> name pkg

  let command =
        ["gzc"]
          <> gzcFlags
          <> (Text.pack <$> modules)
          <> ("-I" : intersperse "-I" (Text.pack <$> include))
          <> ["-ddump-dir=" <> Text.pack (dotRift path), "--keep-zco", "--keep-zci"]

  if dryRun
    then do
      liftIO $ putStrLn $ unwords $ Text.unpack <$> command
      pure ()
    else undefined

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS -Wno-name-shadowing #-}
{-# OPTIONS -Wno-overlapping-patterns #-}

module Rift.Commands.Impl.NewProject (newProjectCommand) where

import Control.Monad (when, unless)
import Control.Monad.Extra (whenM, unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.List as List
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Rift.Config.PackageSet (readLTSVersion)
import Rift.Config.Template
import Rift.Environment (Environment(..))
import qualified Rift.Logger as Logger

import System.Directory (doesDirectoryExist, listDirectory, createDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Text.RawString.QQ (r)
import Turtle (ExitCode(..), procStrictWithErr, empty)


newProjectCommand :: (MonadIO m) => FilePath -> Maybe Text -> Maybe Text -> Bool -> Environment -> m ()
newProjectCommand path name template force Env{..} = do
  unlessM (liftIO $ doesDirectoryExist path) do
    Logger.error $ "Cannot create project in path '" <> Text.pack path <> "' because it is not a directory."
    liftIO exitFailure
  whenM ((&& not force) . not <$> isDirectoryEmpty path) do
    Logger.error "Cannot create project in a non-empty directory, unless '--force' is used."
    liftIO exitFailure

  let projectName     = fromMaybe "<my-project>" name
      projectTemplate = resolveTemplate (fromMaybe "executable" template)

  projectTemplate <- maybe (Logger.error ("Template '" <> fromMaybe "executable" template <> "' not found.\nRun 'rift project template list' for a list of all templates.")
                            *> liftIO exitFailure)
                           pure projectTemplate

  Logger.info $ "Initializing empty project in directory '" <> Text.pack path <> "'"

  (exit, out, err) <- procStrictWithErr (Text.pack git) [ "tag", "-l", "-n", "1", "--color", "never" ] empty
  unless (exit == ExitSuccess) do
    Logger.error $ "'git' process returned non 0 exit code.\n* Standard output:\n"
                                                              <> Text.unlines (mappend "> " <$> Text.lines out) <> "\n* Standard error:\n"
                                                              <> Text.unlines (mappend "> " <$> Text.lines err)
    liftIO exitFailure

  let allLTSs = reverse $ List.sort $ catMaybes $ readLTSVersion <$> Text.lines out
  lastLTS <- case allLTSs of
    []        -> do
      Logger.warn "No LTS found in current package set.\nDefaulting to the 'unstable' LTS."
      pure "unstable"
    lastLTS:_ -> pure $ Text.pack $ show lastLTS

  -- INFO: Directory structure:
  -- > .
  -- > ├┬ src
  -- > │└─ Main.zc
  -- > ├─ .gitignore
  -- > ├─ LICENSE
  -- > ├─ project.dhall
  -- > └─ README.md

  liftIO do
    createDirectory $ path </> "src"
    Text.writeFile (path </> ".gitignore") gitignoreTemplate
    Text.writeFile (path </> "README.md") $ readmeTemplate projectName
    Text.writeFile (path </> "project.dhall") $ projectDhallTemplate projectName projectTemplate lastLTS
    when (projectTemplate == Executable) do
      Text.writeFile (path </> "src" </> "Main.zc") $ mainZCTemplate projectName

  Logger.info "New project successfully initialized!"
  where
    isDirectoryEmpty = liftIO . fmap null . listDirectory



---- TEMPLATES

gitignoreTemplate :: Text
gitignoreTemplate = [r|# Rift local directory
.rift/

# Unix object files/executables
*.o
*.out

# Windows object files/executables
*.a
*.exe
|]

readmeTemplate :: Text -> Text
readmeTemplate = mappend "# "

mainZCTemplate :: Text -> Text
mainZCTemplate projectName = [r|export (main)

import Data.Unit (type unit)
import System.IO (puts, effect io)

let main() : io unit :=
  puts("Hello |] <> projectName <> [r|!")
|]

projectDhallTemplate :: Text -> Template -> Text -> Text
projectDhallTemplate projectName projectTemplate lastLTS =
  let componentTemplate = case projectTemplate of
        Executable -> [r|[ Component::{
        , name = "|] <> projectName <> [r|"
        , version = "1.0.0"
        , source-dirs = [ "src" ]
        , kind = Component.Kind.Executable
        }
      ]|]
        Library    -> [r|[ Component::{
        , name = "|] <> projectName <> [r|"
        , version = "1.0.0"
        , source-dirs = [ "src" ]
        , kind = Component.Kind.Library
        }
      ]|]
        Empty      -> [r|[] : List Component.Type|]
        _          -> "{- don't know -}"

  in [r|let Cfg =
        env:RIFT_CFG
      ? https://raw.githubusercontent.com/zilch-lang/rift/master/default-config.dhall

let Project = Cfg.Project

let Version = Cfg.VersionRange

let Component = Cfg.Component

let Dependency = Cfg.Dependency

in  Project::{
    , lts = "|] <> lastLTS <> [r|"
    , components =
      |] <> componentTemplate <> [r|
    }

|]

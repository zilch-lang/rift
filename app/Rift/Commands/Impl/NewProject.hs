{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wno-name-shadowing #-}
{-# OPTIONS -Wno-overlapping-patterns #-}

module Rift.Commands.Impl.NewProject (newProjectCommand) where

import Control.Monad (unless, when)
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Dhall.Core (Directory (..), File (..), Scheme (..), pretty)
import qualified Dhall.Core as Dhall (Binding (..), Chunks (..), Expr (..), FieldSelection (..), Import (..), ImportHashed (..), ImportMode (..), ImportType (..), RecordField (..), URL (..), Var (..))
import Rift.Config.PackageSet (LTSVersion (..), readLTSVersion)
import Rift.Config.Template
import Rift.Environment (Environment (..))
import qualified Rift.Logger as Logger
import System.Directory (createDirectory, doesDirectoryExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Text.RawString.QQ (r)
import Turtle (ExitCode (..), empty, procStrictWithErr)

newProjectCommand :: (MonadIO m) => FilePath -> Maybe Text -> Maybe Text -> Bool -> Environment -> m ()
newProjectCommand path name template force Env {..} = do
  unlessM (liftIO $ doesDirectoryExist path) do
    Logger.error $ "Cannot create project in path '" <> Text.pack path <> "' because it is not a directory."
    liftIO exitFailure
  whenM ((&& not force) . not <$> isDirectoryEmpty path) do
    Logger.error "Cannot create project in a non-empty directory, unless '--force' is used."
    liftIO exitFailure

  let projectName = fromMaybe "<my-project>" name
      projectTemplate = resolveTemplate (fromMaybe "executable" template)

  projectTemplate <-
    maybe
      ( Logger.error ("Template '" <> fromMaybe "executable" template <> "' not found.\nRun 'rift project template list' for a list of all templates.")
          *> liftIO exitFailure
      )
      pure
      projectTemplate

  Logger.info $ "Initializing empty project in directory '" <> Text.pack path <> "'"

  (exit, out, err) <- procStrictWithErr (Text.pack git) ["tag", "-l", "-n", "1", "--color", "never"] empty
  unless (exit == ExitSuccess) do
    Logger.error $
      "'git' process returned non 0 exit code.\n* Standard output:\n"
        <> Text.unlines (mappend "> " <$> Text.lines out)
        <> "\n* Standard error:\n"
        <> Text.unlines (mappend "> " <$> Text.lines err)
    liftIO exitFailure

  let allLTSs = reverse $ List.sort $ catMaybes $ readLTSVersion <$> Text.lines out
  lastLTS <- case allLTSs of
    [] -> do
      Logger.warn "No LTS found in current package set.\nDefaulting to the 'unstable' LTS."
      pure "unstable"
    lastLTS : _ -> pure $ Text.pack $ show lastLTS

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
gitignoreTemplate =
  [r|# Rift local directory
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
mainZCTemplate projectName =
  [r|open import data::unit::(unit)
open import system::io::(puts, io)

public
let main() : io unit :=
  puts("Hello |]
    <> projectName
    <> [r|!")
|]

projectDhallTemplate :: Text -> Template -> Text -> Text
projectDhallTemplate projectName projectTemplate lastLTS =
  let componentTemplate = case projectTemplate of
        Executable ->
          Dhall.ListLit
            Nothing
            [ Dhall.RecordCompletion
                (Dhall.Var $ Dhall.V "Component" 0)
                ( Dhall.RecordLit
                    [ ("name", Dhall.RecordField Nothing (Dhall.TextLit $ Dhall.Chunks [] projectName) Nothing Nothing),
                      ("version", Dhall.RecordField Nothing (Dhall.TextLit $ Dhall.Chunks [] "1.0.0") Nothing Nothing),
                      ("source-dirs", Dhall.RecordField Nothing (Dhall.ListLit Nothing [Dhall.TextLit $ Dhall.Chunks [] "src"]) Nothing Nothing),
                      ("kind", Dhall.RecordField Nothing (Dhall.Field (Dhall.Field (Dhall.Var $ Dhall.V "Component" 0) (Dhall.FieldSelection Nothing "Kind" Nothing)) (Dhall.FieldSelection Nothing "Executable" Nothing)) Nothing Nothing)
                    ]
                )
            ]
        Library ->
          Dhall.ListLit
            Nothing
            [ Dhall.RecordCompletion
                (Dhall.Var $ Dhall.V "Component" 0)
                ( Dhall.RecordLit
                    [ ("name", Dhall.RecordField Nothing (Dhall.TextLit $ Dhall.Chunks [] projectName) Nothing Nothing),
                      ("version", Dhall.RecordField Nothing (Dhall.TextLit $ Dhall.Chunks [] "1.0.0") Nothing Nothing),
                      ("source-dirs", Dhall.RecordField Nothing (Dhall.ListLit Nothing [Dhall.TextLit $ Dhall.Chunks [] "src"]) Nothing Nothing),
                      ("kind", Dhall.RecordField Nothing (Dhall.Field (Dhall.Field (Dhall.Var $ Dhall.V "Component" 0) (Dhall.FieldSelection Nothing "Kind" Nothing)) (Dhall.FieldSelection Nothing "Library" Nothing)) Nothing Nothing)
                    ]
                )
            ]
        Empty ->
          Dhall.ListLit
            (Just $ Dhall.App Dhall.List $ Dhall.Field (Dhall.Var $ Dhall.V "Component" 0) (Dhall.FieldSelection Nothing "Type" Nothing))
            []
        _ -> Dhall.Assert $ Dhall.BoolLit False

      env = Dhall.Embed $ Dhall.Import (Dhall.ImportHashed Nothing $ Dhall.Env "RIFT_CFG") Dhall.Code

      link = Dhall.Embed $ Dhall.Import (Dhall.ImportHashed Nothing $ Dhall.Remote $ Dhall.URL HTTPS "raw.githubusercontent.com" (File (Directory ["master", "rift", "zilch-lang"]) "default-config.dhall") Nothing Nothing) Dhall.Code
   in pretty $
        Dhall.Let
          (Dhall.Binding Nothing "Cfg" Nothing Nothing Nothing $ Dhall.ImportAlt env link)
          $ Dhall.Let
            (Dhall.Binding Nothing "Project" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Project" Nothing))
            $ Dhall.Let
              (Dhall.Binding Nothing "Version" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Version" Nothing))
              $ Dhall.Let
                (Dhall.Binding Nothing "Component" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Component" Nothing))
                $ Dhall.Let
                  (Dhall.Binding Nothing "Dependency" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Dependency" Nothing))
                  $ Dhall.Let
                    (Dhall.Binding Nothing "LTS" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "LTS" Nothing))
                    $ Dhall.RecordCompletion
                      (Dhall.Var $ Dhall.V "Project" 0)
                      $ Dhall.RecordLit
                        [ ("lts", Dhall.RecordField Nothing (toDhall lastLTS) Nothing Nothing),
                          ("components", Dhall.RecordField Nothing (componentTemplate) Nothing Nothing)
                        ]
  where
    toDhall lts = case readLTSVersion lts of
      Just (LTS major minor) ->
        Dhall.App
          ( Dhall.App
              (Dhall.Field (Dhall.Var $ Dhall.V "LTS" 0) (Dhall.FieldSelection Nothing "stable" Nothing))
              (Dhall.NaturalLit $ fromIntegral major)
          )
          (Dhall.NaturalLit $ fromIntegral minor)
      _ -> Dhall.Field (Dhall.Var $ Dhall.V "LTS" 0) (Dhall.FieldSelection Nothing "unstable" Nothing)

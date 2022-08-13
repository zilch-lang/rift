{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wno-name-shadowing #-}
{-# OPTIONS -Wno-overlapping-patterns #-}

module Rift.Commands.Impl.NewProject (newProjectCommand) where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Dhall.Core (Directory (..), File (..), Scheme (..), pretty)
import qualified Dhall.Core as Dhall (Binding (..), Chunks (..), Directory (..), Expr (..), FieldSelection (..), File (..), FilePrefix (..), Import (..), ImportHashed (..), ImportMode (..), ImportType (..), RecordField (..), URL (..), Var (..))
import Rift.Commands.Impl.Utils.Paths (envDhall, projectDhall, riftDhall)
import Rift.Config.PackageSet (LTSVersion (..), readLTSVersion)
import Rift.Config.Template
import Rift.Environment (Environment (..))
import Rift.Internal.Exceptions (RiftException (..))
import qualified Rift.Logger as Logger
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((<.>), (</>))
import Text.RawString.QQ (r)
import Turtle (ExitCode (..), empty, procStrictWithErr)

newProjectCommand :: (?logLevel :: Int, MonadIO m) => FilePath -> Maybe Text -> Maybe Text -> Bool -> Environment -> m ()
newProjectCommand path name template force Env {..} = do
  unlessM (liftIO $ doesDirectoryExist path) do
    liftIO $ throwIO (CannotCreateProjectInNonDirectory path)
  whenM ((&& not force) . not <$> isDirectoryEmpty path) do
    liftIO $ throwIO CannotCreateProjectInNonEmptyDirectory

  let projectName = fromMaybe "<my-project>" name
      projectTemplate = resolveTemplate (fromMaybe "executable" template)

  projectTemplate <-
    liftIO $
      maybe
        (throwIO $ UnknownProjectTemplate template)
        pure
        projectTemplate

  Logger.info $ "Initializing empty project in directory '" <> Text.pack path <> "'"

  (exit, out, err) <- procStrictWithErr (Text.pack git) ["tag", "-l", "-n", "1", "--color", "never"] empty
  unless (exit == ExitSuccess) do
    liftIO $ throwIO $ ExternalCommandError "'git' process returned non 0 exit code." out err

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
  -- > ├─ env.dhall
  -- > ├─ LICENSE
  -- > ├─ project.dhall
  -- > ├─ README.md
  -- > └─ rift.dhall

  liftIO do
    createDirectoryIfMissing True $ path </> "src"
    Text.writeFile (path </> ".gitignore") gitignoreTemplate
    Text.writeFile (path </> "README" <.> "md") $ readmeTemplate projectName
    Text.writeFile (path </> projectDhall) $ projectDhallTemplate projectName projectTemplate
    Text.writeFile (path </> envDhall) $ envDhallTemplate
    Text.writeFile (path </> riftDhall) $ riftDhallTemplate lastLTS
    when (projectTemplate == Executable) do
      Text.writeFile (path </> "src" </> "main" <.> "zc") $ mainZCTemplate projectName

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

envDhallTemplate :: Text
envDhallTemplate =
  let env = Dhall.Embed $ Dhall.Import (Dhall.ImportHashed Nothing $ Dhall.Env "RIFT_CFG") Dhall.Code

      link = Dhall.Embed $ Dhall.Import (Dhall.ImportHashed Nothing $ Dhall.Remote $ Dhall.URL HTTPS "raw.githubusercontent.com" (File (Directory ["master", "rift", "zilch-lang"]) "default-config.dhall") Nothing Nothing) Dhall.Code
   in pretty $ Dhall.ImportAlt env link

riftDhallTemplate :: Text -> Text
riftDhallTemplate lastLTS =
  pretty $
    Dhall.Let
      (Dhall.Binding Nothing "Cfg" Nothing Nothing Nothing $ Dhall.Embed $ Dhall.Import (Dhall.ImportHashed Nothing $ Dhall.Local Dhall.Here $ Dhall.File (Dhall.Directory []) "env.dhall") Dhall.Code)
      $ Dhall.Let
        (Dhall.Binding Nothing "LTS" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "LTS" Nothing))
        $ Dhall.Let
          (Dhall.Binding Nothing "ExtraDependency" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "ExtraDependency" Nothing))
          $ Dhall.Let
            (Dhall.Binding Nothing "Configuration" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Configuration" Nothing))
            $ Dhall.Let
              (Dhall.Binding Nothing "Version" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Version" Nothing))
              $ Dhall.Let
                (Dhall.Binding Nothing "Source" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Source" Nothing))
                $ Dhall.RecordCompletion
                  (Dhall.Var $ Dhall.V "Configuration" 0)
                  ( Dhall.RecordLit
                      [("lts", Dhall.RecordField Nothing (toDhall lastLTS) Nothing Nothing)]
                  )
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

projectDhallTemplate :: Text -> Template -> Text
projectDhallTemplate projectName projectTemplate =
  let v100 = Dhall.App (Dhall.App (Dhall.App (Dhall.Field (Dhall.Var $ Dhall.V "Version" 0) (Dhall.FieldSelection Nothing "v" Nothing)) (Dhall.NaturalLit 1)) (Dhall.NaturalLit 0)) (Dhall.NaturalLit 0)

      component = case projectTemplate of
        Executable ->
          Dhall.RecordLit $
            [ ( projectName,
                Dhall.RecordField
                  Nothing
                  ( Dhall.RecordCompletion
                      (Dhall.Var $ Dhall.V "Component" 0)
                      ( Dhall.RecordLit
                          [ ("version", Dhall.RecordField Nothing v100 Nothing Nothing),
                            ("source-dirs", Dhall.RecordField Nothing (Dhall.ListLit Nothing [Dhall.TextLit $ Dhall.Chunks [] "src"]) Nothing Nothing),
                            ("kind", Dhall.RecordField Nothing (Dhall.Field (Dhall.Field (Dhall.Var $ Dhall.V "Component" 0) (Dhall.FieldSelection Nothing "Kind" Nothing)) (Dhall.FieldSelection Nothing "Executable" Nothing)) Nothing Nothing)
                          ]
                      )
                  )
                  Nothing
                  Nothing
              )
            ]
        Library ->
          Dhall.RecordLit $
            [ ( projectName,
                Dhall.RecordField
                  Nothing
                  ( Dhall.RecordCompletion
                      (Dhall.Var $ Dhall.V "Component" 0)
                      ( Dhall.RecordLit
                          [ ("version", Dhall.RecordField Nothing v100 Nothing Nothing),
                            ("source-dirs", Dhall.RecordField Nothing (Dhall.ListLit Nothing [Dhall.TextLit $ Dhall.Chunks [] "src"]) Nothing Nothing),
                            ("kind", Dhall.RecordField Nothing (Dhall.Field (Dhall.Field (Dhall.Var $ Dhall.V "Component" 0) (Dhall.FieldSelection Nothing "Kind" Nothing)) (Dhall.FieldSelection Nothing "Library" Nothing)) Nothing Nothing)
                          ]
                      )
                  )
                  Nothing
                  Nothing
              )
            ]
        Empty -> Dhall.RecordLit []
        _ -> Dhall.Assert $ Dhall.BoolLit False
   in pretty $
        Dhall.Let
          (Dhall.Binding Nothing "Cfg" Nothing Nothing Nothing $ Dhall.Embed $ Dhall.Import (Dhall.ImportHashed Nothing $ Dhall.Local Dhall.Here $ Dhall.File (Dhall.Directory []) "env.dhall") Dhall.Code)
          $ Dhall.Let
            (Dhall.Binding Nothing "Project" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Project" Nothing))
            $ Dhall.Let
              (Dhall.Binding Nothing "Version" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Version" Nothing))
              $ Dhall.Let
                (Dhall.Binding Nothing "Component" Nothing Nothing Nothing $ Dhall.Field (Dhall.Var $ Dhall.V "Cfg" 0) (Dhall.FieldSelection Nothing "Component" Nothing))
                $ component

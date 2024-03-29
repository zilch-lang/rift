{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Internal.Exceptions where

import Control.Exception (Exception)
import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import Rift.Commands.Impl.Utils.Paths (projectDhall)
import Rift.Config.Package (Package (name, src))
import Rift.Config.PackageSet (LTSVersion)
import Rift.Config.Source (Source (..), prettyLocation, prettySource)
import Rift.Config.Version (SemVer)

data RiftException
  = LTSNotFound
      LTSVersion
  | NoSuchComponent
      Text
  | CannotCreateProjectInNonEmptyDirectory
  | CannotCreateProjectInNonDirectory
      FilePath
  | UnknownProjectTemplate
      (Maybe Text)
  | InconsistentComponentVersions
      Text
      SemVer
      SemVer
  | NoSuchPackage
      Text
      LTSVersion
  | BrokenVersionConstraint
      Text
      Text
  | PackageIsBroken
      Text
      SemVer
  | AmbiguousPackageSources
      Text
      SemVer
      [Package]
  | DependencyCycle
      [Package]
  | InvalidDependencyLocation
      Source
  | Sha256ValidationError
      Text
      Text
      Text
  | ExternalCommandError
      Text
      Text
      Text
  | NotInARiftProject
      FilePath
  | RequestUriIsNotHttpOrHttps
      Text
  | InvalidSemanticVersion
      String
  | CommandFailed

instance Show RiftException where
  show (LTSNotFound lts) =
    "LTS '" <> show lts <> "' not found in the cache.\nPerhaps you want to run 'rift package update'?"
  show (NoSuchComponent name) =
    "Cannot find any component named '" <> Text.unpack name <> "' in the current project."
  show CannotCreateProjectInNonEmptyDirectory =
    "Cannot create a new project in a non-empty directory unless '--force' is specified."
  show (CannotCreateProjectInNonDirectory path) =
    "Path '" <> path <> "' should either refer to a directory or not exist."
  show (UnknownProjectTemplate Nothing) = undefined
  show (UnknownProjectTemplate (Just t)) =
    "'" <> Text.unpack t <> "' is not a known template. Run 'rift project template list' to see a list of valid template names."
  show (InconsistentComponentVersions name ver1 ver2) =
    "Incoherent versions were specified.\nComponent '"
      <> Text.unpack name
      <> "' was expected to have version "
      <> show ver1
      <> " but version "
      <> show ver2
      <> " was found."
  show (NoSuchPackage name lts) =
    "Package '" <> Text.unpack name <> "' not found in LTS '" <> show lts <> "'."
  show (BrokenVersionConstraint name constraintExpr) =
    "Chosen LTS does not have a version of the package '"
      <> Text.unpack name
      <> "' satisfying the given constraint.\n\nWhile checking that the constraint '"
      <> Text.unpack constraintExpr
      <> "' holds."
  show (PackageIsBroken name version) =
    "Package '" <> Text.unpack name <> "' does not have any non-broken source for version " <> show version <> "."
  show (AmbiguousPackageSources name version candidates) =
    foldl'
      (\acc p -> acc <> "\n- " <> Text.unpack (prettySource (src p)))
      ( "Ambiguous package '"
          <> Text.unpack name
          <> "' for version "
          <> show version
          <> ".\nFound the following candidates:"
      )
      candidates
  show (DependencyCycle pkgs) = "A package ended up depending on itself:\n" <> showCycle pkgs
    where
      showCycle = mappend "↳ Package " . showCycle' 2

      showCycle' _ [] = ""
      showCycle' _ [p] = Text.unpack (name p)
      showCycle' n (p : ps) = Text.unpack (name p) <> "\n" <> replicate n ' ' <> "↳ depends on package " <> showCycle' (n + 2) ps
  show (InvalidDependencyLocation source) =
    let (kind, loc) = show' source
     in "Cannot fetch " <> kind <> " from the location " <> Text.unpack (prettyLocation loc)
    where
      show' (Tar loc _) = (".tar archive", loc)
      show' (Zip loc _) = (".zip archive", loc)
      show' (TarGz loc _) = (".tar.gz archive", loc)
      show' (Git loc _) = (".git repository", loc)
      show' (Directory loc) = ("directory", loc)
  show (Sha256ValidationError url expected actual) =
    "Failure validating source '" <> Text.unpack url <> "':\n- Expected SHA256: " <> Text.unpack expected <> "\n- Got SHA256: " <> Text.unpack actual
  show (ExternalCommandError header out err) =
    Text.unpack header
      <> "\n- Standard output:\n"
      <> unlines (mappend ">> " . Text.unpack <$> Text.lines out)
      <> "\n- Standard error:\n"
      <> unlines (mappend ">> " . Text.unpack <$> Text.lines err)
  show (NotInARiftProject path) =
    "The downloaded project (at " <> path <> ") does not appear to be a Rift project.\nReason: The file '" <> projectDhall <> "' is not present."
  show (RequestUriIsNotHttpOrHttps uri) =
    "URIs with non-HTTP(S) schemes are not supported (while trying to fetch file '" <> Text.unpack uri <> "')."
  show (InvalidSemanticVersion megaparsecError) =
    "Invalid semantic version:\n" <> megaparsecError
  show CommandFailed =
    "External command failed. Check its standard error for more information."

instance Exception RiftException

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Config.Version where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Dhall.Marshal.Decode (FromDhall (..), Natural, auto, field, natural, record)
import Dhall.Marshal.Encode (ToDhall (..), encodeField, recordEncoder, (>$<), (>*<))
import GHC.Generics (Generic)
import qualified Rift.Logger as Logger
import System.Directory.Internal.Prelude (exitFailure)
import Text.Megaparsec (MonadParsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL

data PackageDependency
  = Version
      Text
      -- ^ The name of the package.
      VersionConstraint
      -- ^ The version range, in the form of, for example, @>= version@.

instance FromDhall PackageDependency where
  autoWith _ =
    record $
      Version
        <$> field "package" auto
        <*> field "version" auto

data SemVer
  = SemVersion
      Int
      -- ^ major
      Int
      -- ^ minor
      Int
      -- ^ bug fix
  deriving (Generic, Eq)

instance Hashable SemVer

instance Show SemVer where
  show (SemVersion major minor bug) = show major <> "." <> show minor <> "." <> show bug

instance Ord SemVer where
  SemVersion major1 minor1 bug1 < SemVersion major2 minor2 bug2 =
    or
      [ major1 < major2,
        major1 == major2 && minor1 < minor2,
        major1 == major2 && minor1 == minor2 && bug1 < bug2
      ]
  v1 <= v2 = or [v1 < v2, v1 == v2]

instance FromDhall SemVer where
  autoWith _ =
    record $
      SemVersion
        <$> (fromIntegral <$> field "major" natural)
        <*> (fromIntegral <$> field "minor" natural)
        <*> (fromIntegral <$> field "bug" natural)

instance ToDhall SemVer where
  injectWith _ =
    recordEncoder $
      adjust
        >$< encodeField "major"
        >*< encodeField "minor"
        >*< encodeField "bug"
    where
      adjust :: SemVer -> (Natural, (Natural, Natural))
      adjust (SemVersion major minor bug) = (fromIntegral major, (fromIntegral minor, fromIntegral bug))

type VersionConstraint = SemVer -> Bool

trueConstraint :: VersionConstraint
trueConstraint = const True

--------------------------------

type Parser m = (MonadParsec Void Text m)

parseSemVer :: (MonadIO m) => Text -> m SemVer
parseSemVer txt = case MP.parse pSemVer "version" txt of
  Left err -> do
    Logger.error $ "Invalid semantic version:\n" <> Text.pack (MP.errorBundlePretty err)
    liftIO exitFailure
  Right ver -> pure ver

pSemVer :: (Parser m) => m SemVer
pSemVer =
  SemVersion
    <$> (MPL.decimal <* MPC.char '.')
    <*> (MPL.decimal <* MPC.char '.')
    <*> MPL.decimal

data ConstraintExpr
  = Eq SemVer
  | Neq SemVer
  | Lt SemVer
  | Gt SemVer
  | Le SemVer
  | Ge SemVer
  | And ConstraintExpr ConstraintExpr
  | Or ConstraintExpr ConstraintExpr

parseVersionConstraint :: (MonadIO m) => Text -> m VersionConstraint
parseVersionConstraint txt = case MP.parse pVersionConstraint "constraint" txt of
  Left err -> do
    Logger.error $ "Invalid version constraint:\n" <> Text.pack (MP.errorBundlePretty err)
    liftIO exitFailure
  Right pred -> pure pred

pVersionConstraint :: forall m. (Parser m) => m VersionConstraint
pVersionConstraint = interpret <$> makeExprParser (pUnary <* MP.many MPC.space) [[and, or]]
  where
    pUnary = MP.choice [eq pSemVer, neq pSemVer, gt pSemVer, lt pSemVer, ge pSemVer, le pSemVer]

    eq, neq, gt, lt, ge, le :: (Parser m) => m SemVer -> m ConstraintExpr
    eq ver = Eq <$> (MPC.string "==" *> MP.many MPC.space *> ver)
    neq ver = Neq <$> (MPC.string "!=" *> MP.many MPC.space *> ver)
    gt ver = Gt <$> (MPC.string ">" *> MP.many MPC.space *> ver)
    lt ver = Lt <$> (MPC.string "<" *> MP.many MPC.space *> ver)
    ge ver = Ge <$> (MPC.string ">=" *> MP.many MPC.space *> ver)
    le ver = Le <$> (MPC.string "<=" *> MP.many MPC.space *> ver)

    and, or :: (Parser m) => Operator m ConstraintExpr
    and = InfixN do
      void $ MPC.string "&&" <* MP.many MPC.space
      pure And
    or = InfixN do
      void $ MPC.string "||" <* MP.many MPC.space
      pure Or

-- | Transform a constraint expression into an actuall 'SemVer' unary predicate.
interpret :: ConstraintExpr -> VersionConstraint
interpret (Eq v2) = (== v2)
interpret (Neq v2) = (/= v2)
interpret (Lt v2) = (< v2)
interpret (Gt v2) = (> v2)
interpret (Le v2) = (<= v2)
interpret (Ge v2) = (>= v2)
interpret (And f g) = (&&) `on2` (interpret f, interpret g)
interpret (Or f g) = (||) `on2` (interpret f, interpret g)

on2 :: (b -> c -> d) -> (a -> b, a -> c) -> a -> d
on2 f (g, h) = \x -> f (g x) (h x)

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Config.Version.Parser where

import Control.Exception (throwIO)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Rift.Config.Version (SemVer (..), VersionConstraint)
import Rift.Internal.Exceptions (RiftException (..))
import Text.Megaparsec (MonadParsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL

type Parser m = (MonadParsec Void Text m)

parseSemVer :: (MonadIO m) => Text -> m SemVer
parseSemVer txt = case MP.parse pSemVer "version" txt of
  Left err -> liftIO $ throwIO $ InvalidSemanticVersion (MP.errorBundlePretty err)
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

instance Show ConstraintExpr where
  show expr = "λ(ver : Version.Type) → " <> show' expr
    where
      show' (Eq ver) = "Version.`==` ver (Version.v " <> replace '.' ' ' (show ver) <> ")"
      show' (Neq ver) = "Version.`!=` ver (Version.v " <> replace '.' ' ' (show ver) <> ")"
      show' (Lt ver) = "Version.`<` ver (Version.v " <> replace '.' ' ' (show ver) <> ")"
      show' (Gt ver) = "Version.`>` ver (Version.v " <> replace '.' ' ' (show ver) <> ")"
      show' (Le ver) = "Version.`<=` ver (Version.v " <> replace '.' ' ' (show ver) <> ")"
      show' (Ge ver) = "Version.`>=` ver (Version.v " <> replace '.' ' ' (show ver) <> ")"
      show' (And c1 c2) = "(" <> show' c1 <> " && " <> show' c2 <> ")"
      show' (Or c1 c2) = "(" <> show' c1 <> " || " <> show' c2 <> ")"

      replace _ _ [] = []
      replace x y (z : zs)
        | x == z = y : replace x y zs
        | otherwise = z : replace x y zs

parseVersionConstraint :: (MonadIO m) => Text -> m VersionConstraint
parseVersionConstraint txt = case MP.parse pVersionConstraint "constraint" txt of
  Left err -> liftIO $ throwIO $ InvalidSemanticVersion (MP.errorBundlePretty err)
  Right pred -> pure pred

pVersionConstraint :: forall m. (Parser m) => m VersionConstraint
pVersionConstraint = first (Text.pack . show) . apRet interpret <$> makeExprParser (pUnary <* MPC.hspace) [[and, or]]
  where
    pUnary = MP.choice [eq pSemVer, neq pSemVer, ge pSemVer, le pSemVer, gt pSemVer, lt pSemVer]

    eq, neq, gt, lt, ge, le :: (Parser m) => m SemVer -> m ConstraintExpr
    eq ver = Eq <$> (MPC.string "==" *> MPC.hspace *> ver)
    neq ver = Neq <$> (MPC.string "!=" *> MPC.hspace *> ver)
    gt ver = Gt <$> (MPC.string ">" *> MPC.hspace *> ver)
    lt ver = Lt <$> (MPC.string "<" *> MPC.hspace *> ver)
    ge ver = Ge <$> (MPC.string ">=" *> MPC.hspace *> ver)
    le ver = Le <$> (MPC.string "<=" *> MPC.hspace *> ver)

    and, or :: (Parser m) => Operator m ConstraintExpr
    and = InfixN $ And <$ (MPC.hspace *> MPC.string "&&" <* MPC.hspace)
    or = InfixN $ Or <$ (MPC.hspace *> MPC.string "||" <* MPC.hspace)

-- | Transform a constraint expression into an actuall 'SemVer' unary predicate.
interpret :: ConstraintExpr -> SemVer -> Bool
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

apRet :: (a -> b) -> a -> (a, b)
apRet f x = (x, f x)

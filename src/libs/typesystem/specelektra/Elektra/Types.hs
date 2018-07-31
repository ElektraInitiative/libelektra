--
-- @file
--
-- @brief Shared type definitions and utilities
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE LambdaCase #-}

module Elektra.Types (SolveResult (..), RegexType (..), rightToMaybe, RegexEq (..)) where

import Data.Bool (bool)

import qualified FiniteAutomata as FA

import GhcPlugins (TyVar)
import TcRnTypes  (Ct)

data SolveResult = Failure [Ct] | Solved [Ct] | New [Ct] [Ct]

data RegexType = EmptyRegex | Regex String FA.FiniteAutomata | RegexVar TyVar | RegexIntersection RegexType RegexType

instance Show RegexType where
  show (RegexVar _) = "RegexVar _"
  show EmptyRegex = "EmptyRegex"
  show (Regex s _) = "Regex " ++ s
  show (RegexIntersection l r) = "RegexIntersection " ++ show l ++ " " ++ show r

class RegexEq a where
  (===) :: a -> a -> IO Bool

instance Eq RegexType where
  RegexVar a == RegexVar b = a == b
  EmptyRegex == EmptyRegex = True
  Regex a _ == Regex b _ = a == b
  RegexIntersection l1 r1 == RegexIntersection l2 r2 = l1 == l2 && r1 == r2
  _ == _ = False

instance RegexEq RegexType where
  Regex _ a === Regex _ b = bool False True . (== 1) <$> FA.equals a b
  RegexIntersection l1 r1 === RegexIntersection l2 r2 = (&&) <$> l1 === l2 <*> r1 === r2
  a === b = return $ a == b

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

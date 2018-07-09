--
-- @file
--
-- @brief Shared type definitions and utilities
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Types (SolveResult (..), RegexType (..), rightToMaybe) where

import FiniteAutomata (FiniteAutomata)

import GhcPlugins (TyVar)
import TcRnTypes  (Ct)

data SolveResult = Failure [Ct] | Solved [Ct] | New [Ct] [Ct]

data RegexType = EmptyRegex | Regex String FiniteAutomata | RegexVar TyVar | RegexIntersection RegexType RegexType

instance Show RegexType where
  show (RegexVar _) = "RegexVar _"
  show EmptyRegex = "EmptyRegex"
  show (Regex s _) = "Regex " ++ s
  show (RegexIntersection l r) = "RegexIntersection " ++ show l ++ " " ++ show r

instance Eq RegexType where
  RegexVar a == RegexVar b = a == b
  EmptyRegex == EmptyRegex = True
  Regex a _ == Regex b _ = a == b
  RegexIntersection l1 r1 == RegexIntersection l2 r2 = l1 == l2 && r1 == r2
  _ == _ = False

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

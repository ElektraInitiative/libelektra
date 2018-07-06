--
-- @file
--
-- @brief Shared type definitions and utilities
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Types (SolveResult (..), RegexType (..), rightToMaybe) where

import GhcPlugins (TyVar)
import TcRnTypes  (Ct)

data SolveResult = Failure [Ct] | Solved [Ct] | New [Ct] [Ct]

data RegexType = EmptyRegex | Regex String | RegexVar TyVar | RegexIntersection RegexType RegexType deriving Eq

instance Show RegexType where
    show (RegexVar _) = "RegexVar _"
    show EmptyRegex = "EmptyRegex"
    show (Regex s) = "Regex " ++ s
    show (RegexIntersection l r) = "RegexIntersection " ++ show l ++ " " ++ show r

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

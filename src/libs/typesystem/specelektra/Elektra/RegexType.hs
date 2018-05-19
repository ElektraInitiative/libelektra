--
-- @file
--
-- @brief Regex Types for SpecElektra
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TypeInType, TypeFamilies, ConstraintKinds, UndecidableInstances #-}

module Elektra.RegexType (RegexContains, RegexIntersection, RegexIntersects, Key (..)) where

import GHC.TypeLits
import GHC.Exts (Constraint)

-- Not actually undecideable since our typechecker plugin WILL decide RegexContains' in all cases
-- But we need to differentiate between true/false under the hood for the typechecker instead of
-- just contradicting RegexContains directly

-- Is the regex a contained in regex b?
type family RegexContains (a :: Symbol) (b :: Symbol) :: Constraint where
    RegexContains a b = RegexContains' a b ~ 'True
type family RegexContains' (a :: Symbol) (b :: Symbol) :: Bool

-- Can the two regexes be intersected together without becoming empty?
type family RegexIntersects (a :: Symbol) (b :: Symbol) :: Constraint where
    RegexIntersects a b = RegexIntersects' a b ~ 'True
type family RegexIntersects' (a :: Symbol) (b :: Symbol) :: Bool

-- Calculate the regex that represents the intersection of the regexes a and b
type family RegexIntersection (a :: Symbol) (b :: Symbol) :: Symbol

-- A key which may hold a value
newtype Key (a :: Symbol) = Key (Maybe String) deriving Show

--
-- @file
--
-- @brief Regex Types for SpecElektra
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TypeInType, TypeFamilies, ConstraintKinds #-}

module Elektra.RegexType (RegexContains, RegexIntersection, Intersectable, Key (..), intersect, link) where

import GHC.TypeLits
import GHC.Exts (Constraint)

-- Not actually undecideable since our typechecker plugin WILL decide RegexContains' in all cases
-- But we need to differentiate between true/false under the hood for the typechecker instead of
-- just contradicting RegexContains directly

-- Is the regex a contained in regex b?
type family RegexContains (a :: Symbol) (b :: Symbol) :: Constraint
  where -- implemented by the typechecker plugin

link :: RegexContains b a => Key b -> Key a -> Key a
link = undefined

-- Is the given regex non empty?
type family Intersectable (a :: Symbol) :: Constraint
  where -- implemented by the typechecker plugin

intersect :: Intersectable (RegexIntersection a b) => Key a -> Key b -> Key (RegexIntersection a b)
intersect = undefined

-- Calculate the regex that represents the intersection of the regexes a and b
type family RegexIntersection (a :: Symbol) (b :: Symbol) :: Symbol
  where -- implemented by the typechecker plugin

-- A key
data Key (a :: Symbol) = Key deriving Show

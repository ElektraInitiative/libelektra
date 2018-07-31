--
-- @file
--
-- @brief Regex Types for SpecElektra
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds #-}

module Elektra.RegexType (RegexContains, RegexIntersection, Intersectable, Key (..),  Regex, intersect, link) where

import GHC.TypeLits
import GHC.Exts (Constraint)

-- Is the regex a contained in regex b?
type family RegexContains (a :: Symbol) (b :: Symbol) :: Constraint
  where
    RegexContains a (".*") = a ~ a
    RegexContains a a = a ~ a
    -- otherwise interpreted by the typechecker plugin

link :: RegexContains b a => Key b -> Key a -> Key a
link = undefined

-- Is the given regex non empty?
type family Intersectable (a :: Symbol) :: Constraint
  where -- interpreted by the typechecker plugin

intersect :: Intersectable (RegexIntersection a b) => Key a -> Key b -> Key (RegexIntersection a b)
intersect = undefined

-- Calculate the regex that represents the intersection of the regexes a and b
type family RegexIntersection (a :: Symbol) (b :: Symbol) :: Symbol
  where
    -- trivial simplifications
    RegexIntersection a a      = a
    RegexIntersection a (".*") = a
    RegexIntersection (".*") b = b
    -- otherwise interpreted by the typechecker plugin

-- A key
data Key (a :: Symbol) = Key deriving Show
type Regex = Key

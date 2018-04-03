{-# LANGUAGE TypeInType, TypeFamilies, ConstraintKinds, UndecidableInstances #-}

module RegexTypes.RegexType (RegexContains, RegexIntersection, RegexIntersects, Key (..)) where

import GHC.TypeLits
import GHC.Exts (Constraint)

-- Not actually undecideable since our typechecker plugin WILL decide RegexContains' in all cases
-- But we need to differentiate between true/false under the hood for the typechecker instead of
-- just contradicting RegexContains directly
type family RegexContains (a :: Symbol) (b :: Symbol) :: Constraint where
    RegexContains a b = RegexContains' a b ~ 'True
type family RegexContains' (a :: Symbol) (b :: Symbol) :: Bool

type family RegexIntersects (a :: Symbol) (b :: Symbol) :: Constraint where
    RegexIntersects a b = RegexIntersects' a b ~ 'True
type family RegexIntersects' (a :: Symbol) (b :: Symbol) :: Bool

type family RegexIntersection (a :: Symbol) (b :: Symbol) :: Symbol

newtype Key (a :: Symbol) = Key (Maybe String) deriving Show

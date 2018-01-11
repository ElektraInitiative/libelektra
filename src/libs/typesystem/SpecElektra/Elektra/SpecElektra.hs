{-# LANGUAGE TypeFamilies, TypeInType, TypeOperators,
             UndecidableInstances, GADTs, ExistentialQuantification, ScopedTypeVariables #-}

module Elektra.SpecElektra (
  Key (..), CheckRestriction (..), BaseType (..), Spec (..), LftType (..),
  eval, type ($), type (<=>), type (?>), type (?+), type (<+>)
) where

import Data.Kind (type (*))

-- Our "Types"
data CheckRestriction = forall a . Check a
                      | CheckRestriction :?+ CheckRestriction

data LftType = In | Out | Same
data BaseType = Top BaseType
              | Any
              | forall a . TyLft LftType a BaseType
              | Sub CheckRestriction
              | Sum BaseType BaseType
              | BaseType :-> BaseType
              | Bot

type Path  = String
type Value = Maybe String
type TopUnit = ('Top 'Bot)

-- *** some funky type level operators that makes it look nicer and less parantheses ***

type f $ a = f a
infixr 2 $

type t <=> u = Intersection t u
infixl 3 <=>

type n ?> m = n $ 'Check m
infixr 3 ?>

type n ?+ m = n ':?+ 'Check m
infixr 3 ?+

type n <+> m = SubExpansion n ('Check m)
infixr 3 <+>

--
-- *** Basic data and type definitions ***
--

-- The GADT that defines our keys and their properties in our type system
-- It is basically an alternative evaluation/representation approach compared to the function approach down below
data Key (a :: BaseType) = Key Path Value deriving Show

-- Our Specifications. We only allow functions specified on our keys to enforce our BaseTypes
-- Haskell Types are not allowed as we want to stay inside our BaseType Kind
-- But in order to allow the optional addition of being able to calculate default values via normal haskell functions
-- we just carry a second representation along. While the second representation would leak out haskell types and values
-- and thus does not match our type system specification, the first parameter does
data Spec (k :: BaseType) (b :: *)
  where
    Lft :: Key a -> Spec a (Key a)
    Lam :: (Spec a (Key a) -> Spec c d) -> Spec (a ':-> c) (Key a -> d)
    App :: Spec (a ':-> c) (Key a -> d) -> Spec a (Key a) -> Spec c d

-- This is not required for the type system, its an optional extension to be able to support haskell functions directly
-- to work with key values
reduce :: Spec a b -> b
reduce (Lft k)   = k
reduce (Lam fn)  = \(x :: Key y) -> reduce (fn (Lft x))
reduce (App f x) = reduce f (reduce x)

eval :: (b ~ Key c) => Spec a b -> Key (TypeFinalizer c)
eval = (\(Key p v) -> Key p v) . reduce

--
-- *** Our type system rules expressed in first order logic ***
--
type family SubExpansion (s :: BaseType) c :: BaseType where
  SubExpansion ('Top TopUnit) x = 'Sub x
  SubExpansion ('Sub s)       x = 'Sub $ s ':?+ x
  SubExpansion _              _ = 'Bot

-- Clears out the top/inferring states and resolves outstanding errors
-- This is used to cleanup logical properties which are only relevant to the type system implementation
type family TypeFinalizer (a :: BaseType) :: BaseType where
  TypeFinalizer ('Top TopUnit)    = 'Any
  TypeFinalizer ('Top a)          = a
  TypeFinalizer ('Sum 'Bot _)     = 'Bot
  TypeFinalizer ('Sum _ 'Bot)     = 'Bot
  TypeFinalizer ('Sum a b)        = 'Sum (TypeFinalizer a) (TypeFinalizer b)
  TypeFinalizer ('TyLft _ _ 'Bot) = 'Bot
  TypeFinalizer ('TyLft a b c)    = ('TyLft a b (TypeFinalizer c))
  TypeFinalizer a                 = a

-- meaing of the rules from top to bottom:
  -- We can't assign anything to out types
  -- If the second one has no information yet, we ignore it and stick to the current one
  -- The first type has absolutely no information yet, so it will use the type of the second
  -- If both are top types, intersect them, it will be at least any
  -- Similar to the previous case where one key has already a fixed type
  -- Similar to the previous case where one key has already a fixed type 
  -- We can assign other Subtypes to Sub in case they match, otherwise its BOT
  -- If c is the type a or b, this sum type assignment works, otherwise not
  -- e.g. a = NotNull b, in case a and b match thats fine and a inherits the property
  -- e.g. NonWrite b = a, we can't assign anything to it
  -- e.g. Encrypted a = Encrypted b, in case they are compatible thats fine
  -- same types unify obviously
  -- We can assign everything to Any
  -- anything else is not inferable/compatible and will result in the invalid bottom type
type family (Intersection) (a :: BaseType) (b :: BaseType) :: BaseType where
  Intersection ('TyLft 'Out _ _)  _                  = 'Bot
  Intersection a                  ('Top ('Top 'Bot)) = a
  Intersection ('Top ('Top 'Bot)) b                  = 'Top b
  Intersection ('Top a)           ('Top b)           = 'Top $ a <=> b || 'Any
  Intersection ('Top a)           b                  = 'Top $ a <=> b || 'Any
  Intersection a                  ('Top b)           = 'Top $ a <=> b || 'Any
  Intersection ('Sub a)           ('Sub b)           = IntersectSubtype a b
  Intersection ('Sum a b)         c                  = IntersectSumtype a b c 
  Intersection c                  ('TyLft 'In a b)   = 'TyLft 'In a $ c <=> b
  Intersection c                  ('TyLft 'Out a b)  = c <=> b
  Intersection ('TyLft t a b)     ('TyLft t a c)     = 'TyLft t a $ b <=>  c
  Intersection a                  a                  = a
  Intersection 'Any               b                  = 'Any
  Intersection _                  _                  = 'Bot 
  -- TypeError (Text "Trying to fallback to a key with an incompatible type")

type family (a :: BaseType) && (b :: BaseType) :: BaseType where
  a && a = a
  _ && _ = 'Bot

type family (a :: BaseType) || (b :: BaseType) :: BaseType where
  'Bot || b    = b
  a    || 'Bot = a
  a    || a    = a
  a    || 'Any = a
  _    || _    = 'Bot

type family (a :: BaseType) ?? (b :: BaseType) :: BaseType where
  a ?? 'Bot = 'Bot
  a ?? b    = a

type family IntersectSumtype (a :: BaseType) (b :: BaseType) (c :: BaseType) :: BaseType where
  IntersectSumtype a        b        a        = 'Sum a b
  IntersectSumtype b        a        a        = 'Sum b a
  IntersectSumtype ('Sub a) ('Sub b) ('Sub c) = 'Sum ('Sub a) ('Sub b) ?? (IntersectSubtype a c || IntersectSubtype b c)
  IntersectSumtype ('Sub a) b        ('Sub c) = 'Sum ('Sub a) b ?? IntersectSubtype a c
  IntersectSumtype a        ('Sub b) ('Sub c) = 'Sum a ('Sub b) ?? IntersectSubtype b c
  IntersectSumtype _        _        _        = 'Bot

type family IntersectSubtype (a :: CheckRestriction) (b :: CheckRestriction) :: BaseType where
  IntersectSubtype a          (a ':?+ b) = 'Sub a
  IntersectSubtype a          (b ':?+ a) = 'Sub a
  IntersectSubtype (a ':?+ b) (c ':?+ d) = IntersectSubtypes a (c ':?+ d) && IntersectSubtypes b (c ':?+ d)
  IntersectSubtype a          (b ':?+ c) = IntersectSubtypes a (b ':?+ c)
  IntersectSubtype a          a          = 'Sub a
  IntersectSubtype _          _          = 'Bot

type family IntersectSubtypes (a :: CheckRestriction) (b :: CheckRestriction) :: BaseType where
  IntersectSubtypes a (a ':?+ b)          = 'Sub a
  IntersectSubtypes a (b ':?+ a)          = 'Sub a
  IntersectSubtypes a ((c ':?+ d) ':?+ _) = IntersectSubtypes a (c ':?+ d)
  IntersectSubtypes a (_ ':?+ (c ':?+ d)) = IntersectSubtypes a (c ':?+ d)
  IntersectSubtypes _ _                   = 'Bot

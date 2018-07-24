--
-- @file
--
-- @brief Normalizes RegexTypes
--
-- Normalizes a RegexType by applying commutativity of intersection.
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE LambdaCase #-}

module Elektra.Normalize (normalize) where

import Control.Monad (foldM)
import Data.Maybe    (maybe, fromJust, fromMaybe)
import Data.Either   (either)
import Data.List     (nub, sortBy)
import Data.Function (on)

import Elektra.Types
import qualified FiniteAutomata as FA

-- Set intersection is associative and commutative
-- Thus we can intersect all concrete regexes in advance
-- and leave the remaining intersections in the order that 
-- variable intersections are first, then a concrete regex
-- follows (or another variable)
normalize :: RegexType -> IO RegexType
normalize i@(RegexIntersection _ _) = either return fold $ collect i ([], [])
  where
    fold (ts, ss) = do
      b <- (\(Right r) -> r) <$> FA.compile ".*"
      e <- FA.makeBasic FA.Empty
      s <- case length ss of
           0 -> return Nothing
           1 -> return . Just $ head ss
           _ -> let ss' = map (\(Regex _ r) -> r) ss in do
            s' <- foldM (FA.intersect) (head ss') (tail ss')
            FA.minimize s'
            isEmpty <- (\case
                          x | x <= 0    -> False
                            | otherwise -> True) <$> FA.equals e s'
            if isEmpty then return $ Just EmptyRegex else Just . maybe EmptyRegex (flip Regex s') . rightToMaybe <$> FA.asRegexp s'
      return $ finalize (sortBy (compare `on` \(RegexVar v) -> v) ts, s)
    finalize (_,  Just EmptyRegex) = EmptyRegex
    finalize ([], Just sr)         = sr
    finalize (ts, Nothing)         = foldTyVars ts
    finalize (ts, Just sr)         = RegexIntersection (foldTyVars ts) sr 
    foldTyVars []     = EmptyRegex
    foldTyVars [x]    = x
    foldTyVars (t:ts) = RegexIntersection t (foldTyVars ts)
    collect (RegexIntersection l r) p = collect r =<< collect l p
    collect r@(Regex _ _)   (ts, ss)  = Right (ts, r : ss)
    collect v@(RegexVar _)  (ts, ss)  = Right (v : ts, ss)
    collect EmptyRegex _              = Left EmptyRegex
normalize r = return r

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight without right value"

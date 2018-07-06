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

import Control.Monad (mapM, foldM)
import Data.Maybe (maybe, catMaybes)
import Data.Either (either)
import Data.List (nub, sort)

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
      putStrLn $ "collected " ++ show (length ts) ++ " vars and " ++ show (length ss) ++ " strings"
      b <- FA.makeBasic FA.Total
      s <- mapM (fmap rightToMaybe . FA.compile) ss >>= foldM FA.intersect b . catMaybes
      e <- FA.makeBasic FA.Empty
      isEmpty <- (\case x | x <= 0    -> False
                          | otherwise -> True)
                 <$> FA.equals e s
      -- intersection with "total" equals to the original result, in that case we can omit that
      ss' <- if isEmpty then return EmptyRegex else maybe EmptyRegex Regex . rightToMaybe <$> FA.asRegexp s
      maybe EmptyRegex (finalize (sort $ nub ts, ss'))
            . (\case x | x < 0     -> Nothing
                       | x == 0    -> Just False
                       | otherwise -> Just True)
            <$> FA.equals b s
    finalize (ts, _) True = foldTyVars ts  
    finalize ([], sr) False = sr
    finalize (_, EmptyRegex) False = EmptyRegex
    finalize (ts, sr) False = RegexIntersection (foldTyVars ts) sr
    foldTyVars [] = Regex ".*"
    foldTyVars [x] = RegexVar x
    foldTyVars (t:ts) = RegexIntersection (RegexVar t) $ foldTyVars ts
    collect (RegexIntersection l r) p = collect r =<< collect l p
    collect (Regex r) (ts, ss) = Right (ts, r : ss)
    collect (RegexVar v) (ts, ss) = Right (v : ts, ss)
    collect EmptyRegex _ = Left EmptyRegex
normalize r = return r

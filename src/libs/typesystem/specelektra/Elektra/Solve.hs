--
-- @file
--
-- @brief Solves or contradicts regex constraints
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE LambdaCase #-}

module Elektra.Solve (regexIntersections, regexContains) where

import Data.Maybe (catMaybes)

import Elektra.Parse
import Elektra.Normalize
import Elektra.Types
import Elektra.GhcPluginExtras

import qualified FiniteAutomata as FA

import GhcPlugins
import TcPluginM  (TcPluginM, tcPluginIO, newWanted, newGiven)
import TcRnTypes  (Ct(..), ctEvidence, ctEvPred, ctLoc, unsafeTcPluginTcM)
import TyCoRep    (Type(..))
import TcMType    (newFskTyVar)

-- Intersection solving works by using intersection commutativity to normalize
-- everything to one big intersection constraint
regexIntersections :: TyCon -> TyCon -> [Ct] -> TcPluginM (Maybe SolveResult)
regexIntersections _ _ [] = return Nothing
regexIntersections rtc1 rtc2 cs = do
    ts <- tcPluginIO $ mapM (fromAST . extract) cs
    let t = foldl1 RegexIntersection ts
    case t of
      -- Intersectable (Regex) is trivially true
      Regex _ _ -> return . Just $ Solved cs
      _         -> do
        -- unify all constraints and calculate a normal form if possible
        n <- tcPluginIO $ normalize t
        -- check if it was already in normal form, we ignore semantic regex differences
        eq <- tcPluginIO $ t === n
        if eq then return Nothing else case n of
          -- Normalization failed
          EmptyRegex -> return . Just $ Failure cs
          _          -> finalize ts n
  where
    extract ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
      IrredPred (TyConApp _ [t]) -> t
      _                          -> error "Expected an IrredPred of Intersectable but got something else instead"
    -- normal form possible, aggregate all possible coercions and emit the result
    finalize ts n = do
      evNormalized <- newWanted (ctLoc $ head cs) (TyConApp rtc1 [toAST rtc2 n])
      coercions    <- catMaybes <$> mapM normalizationConstraints (zip cs ts)
      return . Just . New cs $ CNonCanonical evNormalized : concat coercions
    normalizationConstraints (_, Regex _ _) = return Nothing
    normalizationConstraints (c, t) = do
      -- Introduce possible coercion constraints
      n <- tcPluginIO $ normalize t
      eq <- tcPluginIO $ t === n
      if eq then return Nothing else do
        let n' = toAST rtc2 n
        let ct'@(TyConApp _ [ct]) = ctEvPred $ ctEvidence c
        evRgxTypesCoerce <- newGiven (ctLoc c) (mkPrimEqPred ct n') (evByFiat "specElektra" ct n')
        let i = TyConApp rtc1 [n']
        evIntersectableCoerce <- newGiven (ctLoc c) (mkPrimEqPred ct' i) (evByFiat "specElektra" ct' i)
        fsk  <- unsafeTcPluginTcM $ newFskTyVar typeSymbolKind
        fsk2 <- unsafeTcPluginTcM $ newFskTyVar constraintKind
        return $ Just [CFunEqCan evRgxTypesCoerce rtc2 [typeSymbolKind] fsk
                      ,CFunEqCan evIntersectableCoerce rtc1 [constraintKind] fsk2]    

-- Containment solving works by reducing both sides of the containment and checking whether
-- it is solvable for any known regexes when it comes to intersections, otherwise it has to
-- stay as it is. We cannot unify containment predicates to a single containment predicate 
-- therefore we simply concatenate them.
regexContains :: TyCon -> TyCon -> Ct -> TcPluginM (Maybe SolveResult)
regexContains rctc ritc c = do
    (a, b) <- tcPluginIO $ extract c
    an     <- tcPluginIO $ normalize a
    bn     <- tcPluginIO $ normalize b
    -- normalization failed, unsatisfiable
    if an == EmptyRegex || bn == EmptyRegex then return . Just $ Failure [c] else do
      isContainmentPossible <- tcPluginIO $ containmentPossible an bn
      -- normalization succeeded but the containment can already be decided as unsatisfiable
      if not isContainmentPossible then return . Just $ Failure [c] else do
        aEq <- tcPluginIO $ a === an
        bEq <- tcPluginIO $ b === bn
        if aEq && bEq then do
          -- Both sides already in normal form, check if satisfied
          pac <- tcPluginIO $ primitiveAndContained an bn
          if pac then return . Just $ Solved [c]
          else return Nothing
        else
          let a'  = toAST ritc a
              b'  = toAST ritc b
              an' = toAST ritc an
              bn' = toAST ritc bn
          in do
            -- Otherwise normalize both sides first
            af <- if aEq then return Nothing else normalizationConstraint (a, a', an')
            bf <- if bEq then return Nothing else normalizationConstraint (b, b', bn')
            evNormalized <- newWanted (ctLoc c) (TyConApp rctc [toAST ritc an, toAST ritc bn])
            let original = ctEvPred $ ctEvidence c
            let new      = TyConApp rctc [an', bn']
            evNormalizedCoerce <- newGiven (ctLoc c) (mkPrimEqPred original new) (evByFiat "specElektra" original new)
            fsk <- unsafeTcPluginTcM $ newFskTyVar typeSymbolKind
            return . Just $ New [c] (catMaybes [Just $ CNonCanonical evNormalized
                                               ,Just $ CFunEqCan evNormalizedCoerce rctc [constraintKind] fsk, af, bf])
  where    
    extract ct = case classifyPredType . ctEvPred $ ctEvidence ct of
      IrredPred (TyConApp _ [a, b]) -> (,) <$> fromAST a <*> fromAST b
      _                             -> error "Expected an IrredPred of RegexContains but got something else instead"
    containmentPossible (RegexIntersection _ (Regex _ l)) (RegexIntersection _ (Regex _ r)) = contained l r
    containmentPossible (RegexIntersection _ (Regex _ l)) (Regex _ r) = contained l r
    containmentPossible (Regex _ l) (RegexIntersection _ (Regex _ r)) = contained l r
    containmentPossible (Regex _ l) (Regex _ r) = contained l r
    containmentPossible _ _ = return True
    contained l r = (\case x | x <= 0    -> False
                             | otherwise -> True) <$> FA.contains l r 
    normalizationConstraint (Regex _ _, _, _) = return Nothing
    normalizationConstraint (_, a, n) = do
      evRgxTypesCoerce <- newGiven (ctLoc c) (mkPrimEqPred a n) (evByFiat "specElektra" a n)
      fsk <- unsafeTcPluginTcM $ newFskTyVar typeSymbolKind
      return $ Just $ CFunEqCan evRgxTypesCoerce ritc [typeSymbolKind] fsk
    primitiveAndContained (Regex _ l) (Regex _ r) = contained l r
    primitiveAndContained _ _ = return False

--
-- @file
--
-- @brief Solves or contradicts regex constraints
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Solve (regexIntersections) where

import Data.Maybe (catMaybes)

import Elektra.Parse
import Elektra.Normalize
import Elektra.Types
import Elektra.GhcPluginExtras

import GhcPlugins
import TcPluginM  (TcPluginM, tcPluginIO, newWanted, newGiven)
import TcRnTypes  (Ct(..), ctEvidence, ctEvPred, ctLoc, unsafeTcPluginTcM)
import TyCoRep    (Type(..))
import TcMType    (newFskTyVar)

regexIntersections :: TyCon -> TyCon -> [Ct] -> TcPluginM (Maybe SolveResult)
regexIntersections _ _ [] = return Nothing
regexIntersections rtc1 rtc2 cs = do
    ts <- tcPluginIO $ mapM (fromAST . extract) cs
    let t = foldl1 RegexIntersection ts
    tcPluginIO $ print t
    case t of
      -- Intersectable (Regex) is trivially true
      Regex _ -> return $ Just $ Solved cs
      _       -> do
        -- unify all constraints and calculate a normal form if possible
        n <- tcPluginIO $ normalize t
        tcPluginIO $ print n
        -- check if it was already in normal form
        if t == n then return Nothing else case n of
          EmptyRegex -> return $ Just $ Failure cs
          _          -> finalize ts n
  where
    extract ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
      IrredPred (TyConApp _ [t]) -> t
      _                          -> error "Expected an IrredPred of Intersectable but got something else instead"
    -- normal form possible, aggregate all possible coercions and emit the result
    finalize ts n = do
      evNormalized <- newWanted (ctLoc $ head cs) (TyConApp rtc1 [toAST rtc2 n])
      coercions <- catMaybes <$> mapM normalizedRegexIntersection (zip cs ts)
      return . Just . New cs $ CNonCanonical evNormalized : concat coercions
    -- a single regex is trivially intersectable i.e. not empty
    normalizedRegexIntersection (_, Regex _) = return Nothing
    normalizedRegexIntersection (c, t) = do
      -- Introduce possible coercion constraints
      n <- tcPluginIO $ normalize t
      if n == t then return Nothing else do
        let n' = toAST rtc2 n
        let ct'@(TyConApp _ [ct]) = ctEvPred $ ctEvidence c
        evRgxTypesCoerce <- newGiven (ctLoc c) (mkPrimEqPred ct n') (evByFiat "specElektra" ct n')
        let i = TyConApp rtc1 [n']
        evIntersectableCoerce <- newGiven (ctLoc c) (mkPrimEqPred ct' i) (evByFiat "specElektra" ct' i)
        fsk <- unsafeTcPluginTcM $ newFskTyVar typeSymbolKind
        fsk2 <- unsafeTcPluginTcM $ newFskTyVar constraintKind
        return $ Just [CFunEqCan evRgxTypesCoerce rtc2 [typeSymbolKind] fsk
                      ,CFunEqCan evIntersectableCoerce rtc1 [constraintKind] fsk2]      

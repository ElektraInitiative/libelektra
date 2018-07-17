--
-- @file
--
-- @brief Regex Typechecker Plugin for GHC
--
-- A typechecker plugin which treats the semantics of the type operators defined in
-- the Elektra.RegexType module.
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TupleSections #-}

module Elektra.RegexTypeChecker (plugin) where

import Data.Maybe (catMaybes, mapMaybe)

import Elektra.Types
import Elektra.Solve
import Elektra.GhcPluginExtras

-- GHC API
import GhcPlugins
import TcPluginM  (TcPluginM, lookupOrig, tcLookupTyCon)
import TcRnTypes  (Ct(..), TcPlugin(..), TcPluginResult(..), ctEvidence, ctEvPred)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm (..))
import Type       (classifyPredType)
import TyCoRep    (Type(..))

-- GHC Plugin Interface definition
plugin :: Plugin
plugin = defaultPlugin {
  tcPlugin = const . Just $ tracePlugin "RegexTypeChecker" regexPlugin
  }

regexPlugin :: TcPlugin
regexPlugin = TcPlugin {
  tcPluginInit  = initTyCons,
  tcPluginSolve = solveRegex,
  tcPluginStop  = const $ return ()
  }

-- Container for holding the type constructors defined in Elektra.RegexType
data RgxData = RgxData {
  keyTyCon              :: TyCon,
  rgxContainsTyCon      :: TyCon,
  rgxIntersectableTyCon :: TyCon,
  rgxIntersectionTyCon  :: TyCon
}

-- Get the type constructor definitions from this package to filter for them
-- during the typechecking phase
initTyCons :: TcPluginM RgxData
initTyCons = let getRgxTyCon = getTyCon "Elektra.RegexType" "specelektra" in
    RgxData <$> getRgxTyCon "Key"
            <*> getRgxTyCon "RegexContains"
            <*> getRgxTyCon "Intersectable"
            <*> getRgxTyCon "RegexIntersection"

getTyCon :: String -> String -> String -> TcPluginM TyCon
getTyCon m p t = lookupModule regexModule regexPackage
                 >>= flip lookupOrig (mkTcOcc t)
                 >>= tcLookupTyCon
  where
    regexModule  = mkModuleName m
    regexPackage = fsLit p

-- Deal with the semantics of the type operators introduced in the Elektra.RegexType module
-- by either proving or disproving them. This way we can typecheck generated specifications
-- specified in our EDSL.
-- RegexType TyCon -> Given Constraints -> Derived Constraints -> Wanted Constraints -> Result
-- G -> proven facts we can rely on
-- D -> additional facts for error message backtracking
-- W -> facts that the compiler wants us to prove or disprove
solveRegex :: RgxData -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveRegex rd [] _ w  = do
  (rcs, rcf, rcn) <- partitionSolutions . catMaybes <$> pRgxContains
  (ris, rif, rin) <- partitionSolutions . catMaybes . (:[]) <$> pRgxIntersects
  let (slvd, fld, new) = (rcs ++ ris, rcf ++ rif, rcn ++ rin)
  case fld of
    [] -> return $ TcPluginOk (mapMaybe (\c -> (,c) <$> evRegexConstraint c) slvd) new
    _  -> return $ TcPluginContradiction fld
  where
    -- Filter and solve or disprove the type constrains introduced by RegexContains 
    wRgxContains   = filter (hasTyCon $ rgxContainsTyCon rd) w
    pRgxContains   = mapM (regexContains (rgxContainsTyCon rd) (rgxIntersectionTyCon rd)) wRgxContains
    -- Filter and solve or disprove the type constrains introduced by RegexIntersects 
    wRgxIntersects = filter (hasTyCon $ rgxIntersectableTyCon rd) w
    pRgxIntersects = regexIntersections (rgxIntersectableTyCon rd) (rgxIntersectionTyCon rd) wRgxIntersects
-- Ok, nothing we can do
solveRegex _ _ _ _  = return $ TcPluginOk [] []

hasTyCon :: TyCon -> Ct -> Bool
hasTyCon wtc ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _  (TyConApp tc _) _ -> tc == wtc
    IrredPred (TyConApp tc _)   -> tc == wtc
    _                           -> False

evRegexConstraint :: Ct -> Maybe EvTerm
evRegexConstraint ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _  (TyConApp _ [a]) _    -> Just (evByFiat "specelektra" a a)
    IrredPred (TyConApp _ [a])      -> Just (evByFiat "specelektra" a a)
    EqPred _  (TyConApp _ [a, b]) _ -> Just (evByFiat "specelektra" a b)
    IrredPred (TyConApp _ [a, b])   -> Just (evByFiat "specelektra" a b)
    _                               -> Nothing

partitionSolutions :: [SolveResult] -> ([Ct], [Ct], [Ct])
partitionSolutions = foldl collect ([], [], [])
  where
    collect (s, f, n) r = case r of
      (Solved ss)  -> (ss ++ s, f, n)
      (Failure fs) -> (s, fs ++ f, n)
      (New ss ns)  -> (ss ++ s, f, ns ++ n)

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
{-# LANGUAGE CPP, TupleSections, RecordWildCards, LambdaCase #-}

module Elektra.RegexTypeChecker (plugin) where

import Data.List (partition)
import Data.Maybe (mapMaybe, catMaybes)
import Control.Arrow ((***))
import Control.Applicative (liftA2)
import Control.Monad (join)
import Debug.Trace

import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)

import qualified FiniteAutomata as FA

-- GHC API
import GhcPlugins
import Outputable (Outputable (..), (<+>), text, ppr, fcat)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm (..))
import TcPluginM  (TcPluginM, tcLookupTyCon, tcPluginIO, newWanted)
import TcRnTypes  (Ct(..), TcPlugin(..), TcPluginResult(..), ctEvidence, ctEvPred, ctLoc,
                   tyCoVarsOfCtList)
import Type       (classifyPredType, eqType)
import TyCoRep    (TyLit(..), Type(..))

-- GHC Plugin Interface definition
plugin :: Plugin
plugin = defaultPlugin {
  tcPlugin = const $ Just regexPlugin
  }

regexPlugin :: TcPlugin
regexPlugin = TcPlugin {
  tcPluginInit  = initTyCons,
  tcPluginSolve = solveRegex,
  tcPluginStop  = const (return ())
  }

-- Container for holding the type constructors defined in Elektra.RegexType
data RgxData = RgxData {
  keyTyCon             :: TyCon,
  rgxContainsTyCon     :: TyCon,
  rgxIntersectsTyCon   :: TyCon,
  rgxIntersectionTyCon :: TyCon
}

-- Get the type constructor definitions from this package to filter for them
-- during the typechecking phase
initTyCons :: TcPluginM RgxData
initTyCons = do
    let getRgxTyCon = getTyCon "Elektra.RegexType" "specelektra"
    ktc   <- getRgxTyCon "Key"
    rctc  <- getRgxTyCon "RegexContains'"
    ritc  <- getRgxTyCon "RegexIntersects'"
    rixtc <- getRgxTyCon "RegexIntersection"
    return RgxData {
      keyTyCon             = ktc,
      rgxContainsTyCon     = rctc,
      rgxIntersectsTyCon   = ritc,
      rgxIntersectionTyCon = rixtc
    }

getTyCon :: String -> String -> String -> TcPluginM TyCon
getTyCon m p t = lookupModule regexModule regexPackage
                 >>= flip lookupName (mkTcOcc t)
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
solveRegex rgxData [] _ w  = do
  (rcs, rcf) <- (map fst *** map fst) . partition snd . catMaybes <$> pRgxContains
  (ris, rif) <- (map fst *** map fst) . partition snd . catMaybes <$> pRgxIntersects
  let (slvd, fld) = (rcs ++ ris, rcf ++ rif)
  case fld of
    [] -> return $ TcPluginOk (mapMaybe (\c -> (,c) <$> evRegexConstraint c) slvd) []
    _  -> return $ TcPluginContradiction fld
  where
    -- Filter and solve or disprove the type constrains introduced by RegexContains 
    wRgxContains   = filter (hasTyCon $ rgxContainsTyCon rgxData) w
    pRgxContains   = mapM (solve regexContains) wRgxContains
    -- Filter and solve or disprove the type constrains introduced by RegexIntersects 
    wRgxIntersects = filter (hasTyCon $ rgxIntersectsTyCon rgxData) w
    pRgxIntersects = mapM (solve regexIntersects) wRgxIntersects

-- Something already given, means we resolved the constraints already
-- that we can use to prove our equalities
-- Now we have to give a proof for the coercion, which we simply do 
-- directly without giving a concrete proof in GHC's core language
-- Instead, the soundness of the type system is proved in the thesis already
solveRegex _ g  _ w  = return $ TcPluginOk (mapMaybe (\c -> (,c) <$> evRegexEquality c) w) []
  where
    processed  = mapMaybe solveRegexEquality (zip g w)
    (slvd,fld) = (map fst *** map fst)
               $ partition snd processed

hasTyCon :: TyCon -> Ct -> Bool
hasTyCon wtc ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _  (TyConApp tc [_, _]) _ -> tc == wtc
    _                                -> False

solve :: (Type -> Type -> TcPluginM (Maybe Bool)) -> Ct -> TcPluginM (Maybe (Ct, Bool))
solve p ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _  (TyConApp _ [x, y]) _ -> do
      res <- p x y
      return $ (ct,) <$> res
    _                               -> return Nothing

-- Intuitively: "Given our proved constraint RegexContains b a, show that b equals to a up to subtyping"
solveRegexEquality :: (Ct, Ct) -> Maybe (Ct, Bool)
solveRegexEquality (g, w) = case classifyPredType $ ctEvPred $ ctEvidence w of
    EqPred _ wx wy -> case classifyPredType $ ctEvPred $ ctEvidence g of
      EqPred _ (TyConApp _ [gx, gy]) _ -> Just (w, wx `eqType` gx && wy `eqType` gy)
      _ -> Nothing
    _ -> Nothing

-- the actual proof that our solved constraint holds
-- evByFiat basically means "believe me i know what i'm doing"
-- we proof this in the thesis' proof part and just use it directly here
evRegexConstraint :: Ct -> Maybe EvTerm
evRegexConstraint ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _ (TyConApp _ [x, y]) _ -> Just (evByFiat "specelektra" x y)
    _                              -> Nothing

evRegexEquality :: Ct -> Maybe EvTerm
evRegexEquality ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _ x y -> Just (evByFiat "specelektra" x y)
    _            -> Nothing

-- A Regex a is subsumed by the Regex b if a|b represents the same as a
-- e.g. aa is contained in a+ as a+ == (aa)|(a+)
-- Lives in the TcPluginM to have access to IO for the libfa bindings
regexContains :: Type -> Type -> TcPluginM (Maybe Bool)
-- Case 1: Both arguments are already regexes
regexContains (LitTy (StrTyLit fsx)) (LitTy (StrTyLit fsy))
  = let sx = unpackFS fsx
        sy = unpackFS fsy
    in tcPluginIO $ do
      hsx <- FA.compile sx
      hsy <- FA.compile sy
      fmap join . sequence $ liftA2 regexContains' (rightToMaybe hsx) (rightToMaybe hsy)
-- Case 2: the lefthand side is an argument of type RegexIntersection
-- therefore we have to calculate the intersection before the typechecking
regexContains (TyConApp _ [a,b]) (LitTy (StrTyLit fsy)) = 
  let sy = unpackFS fsy in do
    hsx <- reduceRegexIntersects a b
    tcPluginIO $ do
      hsy <- FA.compile sy
      fmap join . sequence $ liftA2 regexContains' hsx (rightToMaybe hsy)
-- Case 2: both sides are an argument of type RegexIntersection
-- therefore we have to calculate the intersections before the typechecking
regexContains (TyConApp _ [a,b]) (TyConApp _ [c,d]) = do
  hsx <- reduceRegexIntersects a b
  hsy <- reduceRegexIntersects c d
  tcPluginIO $ fmap join . sequence $ liftA2 regexContains' hsx hsy
regexContains _ _ = return Nothing

regexContains' :: FA.FiniteAutomata -> FA.FiniteAutomata -> IO (Maybe Bool)
regexContains' hsx hsy = 
  (\case
    e | e < 0     -> Nothing
      | e == 0    -> Just False
      | otherwise -> Just True)
  <$> FA.contains hsx hsy

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

regexIntersects :: Type -> Type -> TcPluginM (Maybe Bool)
regexIntersects x y = reduceRegexIntersects x y >>= \i -> tcPluginIO $ do
  emptyFA <- Just <$> FA.makeBasic FA.Empty
  let eq = sequence $ liftA2 FA.equals emptyFA i
  --We return true if they are not equal - this means the intersection is possible
  (>>= \case
      e | e < 0     -> Nothing
        | e == 0    -> Just True
        | otherwise -> Just False) <$> eq

-- Calculates a regex intersection and returns the result as a FiniteAutomata.
reduceRegexIntersects :: Type -> Type -> TcPluginM (Maybe FA.FiniteAutomata)
reduceRegexIntersects (LitTy (StrTyLit fsx)) (LitTy (StrTyLit fsy)) = tcPluginIO $ do
  let sx = unpackFS fsx
      sy = unpackFS fsy
  hsx <- FA.compile sx
  hsy <- FA.compile sy
  sequence . rightToMaybe $ liftA2 FA.intersect hsx hsy
reduceRegexIntersects (TyConApp _ [a,b]) (LitTy (StrTyLit fsy)) =
    reduceRegexIntersects a b >>= \hsx -> tcPluginIO $ 
    FA.compile (unpackFS fsy) >>= sequence . liftA2 FA.intersect hsx . rightToMaybe
reduceRegexIntersects (LitTy (StrTyLit fsx)) (TyConApp _ [a,b]) =
    reduceRegexIntersects a b >>= \hsy -> tcPluginIO $ 
    FA.compile (unpackFS fsx) >>= sequence . liftA2 FA.intersect hsy . rightToMaybe
reduceRegexIntersects (TyConApp _ [a,b]) (TyConApp _ [c, d]) = do
    hsx <- reduceRegexIntersects a b
    hsy <- reduceRegexIntersects c d
    tcPluginIO $ sequence $ liftA2 FA.intersect hsx hsy
reduceRegexIntersects _ _ = return Nothing

solveEquality :: TyCon -> Ct -> TcPluginM [Ct]
solveEquality key ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq a b -> case tyCoVarsOfCtList ct of
      [va, vb] -> do
        k1 <- isKey a va
        k2 <- isKey b vb
        return [k1, k2]
      _ -> return []
    _ -> return []
  where
    isKey t v = do
      let kt = mkTyConApp key [t]
      ev <- newWanted (ctLoc ct) kt
      return $ CTyEqCan ev v kt NomEq

-- Some debugging utilities
debugS :: String -> TcPluginM ()
debugS = tcPluginIO . putStrLn

debug :: SDoc -> TcPluginM ()
debug = debugS . showSDocUnsafe

tcPluginDebug :: String -> SDoc -> TcPluginM ()
tcPluginDebug a b = debug $ text a <+> b

tracePlugin :: String -> TcPlugin -> TcPlugin
tracePlugin s TcPlugin{..} = TcPlugin { tcPluginInit  = traceInit
                                      , tcPluginSolve = traceSolve
                                      , tcPluginStop  = traceStop
                                      }
  where
    traceInit = tcPluginDebug ("tcPluginInit " ++ s) empty >> tcPluginInit
    traceStop  z = tcPluginDebug ("tcPluginStop " ++ s) empty >> tcPluginStop z
    traceSolve z given derived wanted = do
      tcPluginDebug ("tcPluginSolve start " ++ s)
                        (text "given   =" <+> ppr given
                      $$ text "derived =" <+> ppr derived
                      $$ text "wanted  =" <+> ppr wanted)
      r <- tcPluginSolve z given derived wanted
      case r of
        TcPluginOk solved new     -> tcPluginDebug ("tcPluginSolve ok " ++ s)
                                         (text "solved =" <+> ppr solved
                                       $$ text "new    =" <+> ppr new)
        TcPluginContradiction bad -> tcPluginDebug
                                         ("tcPluginSolve contradiction " ++ s)
                                         (text "bad =" <+> ppr bad)
      return r

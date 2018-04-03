{-# LANGUAGE CPP, TupleSections, RecordWildCards #-}

module RegexTypes.Plugin (plugin) where

import Data.List (partition, isPrefixOf, intersect)
import Data.Maybe (mapMaybe)
import Control.Arrow ((***))
import Control.Applicative (liftA2)
import Debug.Trace

import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)

import qualified Language.HaLex.RegExp as H
import qualified Language.HaLex.RegExpParser as H
import qualified Language.HaLex.RegExp2Fa as H
import qualified Language.HaLex.RegExpAsDiGraph as H
import qualified Language.HaLex.Equivalence as H

import qualified Language.HaLex.Dfa as H
import qualified Language.HaLex.Ndfa as H
import qualified Language.HaLex.Minimize as H
import qualified Language.HaLex.FaOperations as H
import qualified Language.HaLex.Fa2RegExp as H
import qualified Language.HaLex.Parser as H

import qualified Language.HaLex.Dfa2MDfa as H

-- GHC API
import GhcPlugins
import Outputable (Outputable (..), (<+>), text, ppr, fcat)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm (..))
import TcPluginM  (TcPluginM, tcLookupTyCon, tcPluginIO, zonkCt, newWanted)
import TcRnTypes  (Ct(..), TcPlugin(..), TcPluginResult(..), CtEvidence (..), ctEvidence, ctEvPred, ctLoc,
                   tyCoVarsOfCtList)
import Type       (classifyPredType, eqType, isStrLitTy)
import TyCoRep    (TyLit(..), Type(..), pprTyLit)

--
-- proposed way:
-- take raw RegexContains constraint and turn it into boolean constraint
-- proof that boolean constraint
-- infer that if the constraint is true, the equality holds
-- check out how to produce good error messages along the way?
--

plugin :: Plugin
plugin = defaultPlugin {
  tcPlugin = const (Just $ tracePlugin "Tracing Regex Type Checker" regexPlugin)
  }

regexPlugin :: TcPlugin
regexPlugin = TcPlugin {
  tcPluginInit  = initTyCons,
  tcPluginSolve = solveRegex,
  tcPluginStop  = const (return ())
  }

data RgxData = RgxData {
  keyTyCon             :: TyCon,
  rgxContainsTyCon     :: TyCon,
  rgxIntersectsTyCon   :: TyCon,
  rgxIntersectionTyCon :: TyCon
}

initTyCons :: TcPluginM RgxData
initTyCons = do
    let getRgxTyCon = getTyCon "RegexTypes.RegexType" "regex-types"
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

-- Solve Regex Type equalities as defined in our specification
-- RegexType TyCon -> Given Constraints -> Derived Constraints -> Wanted Constraints -> Result
-- G -> proven facts we can rely on
-- D -> additional facts for error message backtracking
-- W -> facts that the compiler wants us to prove or disprove
solveRegex :: RgxData -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveRegex rgxData g  _ [] = do
  ncts <- concat <$> mapM generateRgxCntCt g
  return $ TcPluginOk [] []
  where
    generateRgxCntCt ct@(CFunEqCan e f t fsk) = do
      let ty = mkTyConTy $ keyTyCon rgxData
      ev <- newWanted (ctLoc ct) ty
      return $ [CTyEqCan ev fsk ty NomEq]
    generateRgxCntCt _ = return []

solveRegex rgxData [] _ w  = do
    case fld of
      [] -> return $ TcPluginOk (mapMaybe (\c -> (,c) <$> evRegexConstraint c) slvd) []
      _  -> return $ TcPluginContradiction fld
  where
    wRgxContains   = filter (hasTyCon $ rgxContainsTyCon rgxData) w
    pRgxContains   = mapMaybe (solve regexContains) wRgxContains
    (rcs, rcf)     = (map fst *** map fst)
                   $ partition snd pRgxContains
    wRgxIntersects = filter (hasTyCon $ rgxIntersectsTyCon rgxData) w
    pRgxIntersects = mapMaybe (solve $ regexIntersects $ rgxIntersectionTyCon rgxData) wRgxIntersects
    (ris, rif)     = (map fst *** map fst)
                   $ partition snd pRgxIntersects
    (slvd, fld)    = (rcs ++ ris, rcf ++ rif)

-- Something already given, means we resolved the constraints already
-- that we can use to prove our equalities in terms of subtyping
-- However, a != b "a" != "a-z", this only holds if b is coerced to "a-z" before
-- so we need an extra coercion constraint
solveRegex rgxData g  _ w  = do
    return $ TcPluginOk (mapMaybe (\c -> (,c) <$> evRegexEquality c) w) []
  where
    processed  = mapMaybe solveRegexEquality (zip g w)
    (slvd,fld) = (map fst *** map fst)
               $ partition snd processed

hasTyCon wtc ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _  (TyConApp tc [x, y]) t -> trace ("hasTyCon" ++ showSDocUnsafe (ppr wtc) ++ " vs " ++ showSDocUnsafe (ppr tc)) $ tc == wtc
    _ -> False

solve :: (Type -> Type -> Maybe Bool) -> Ct -> Maybe (Ct, Bool)
solve p ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _  (TyConApp _ [x, y]) t -> (ct,) <$> p x y
    _ -> Nothing

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
    EqPred _ (TyConApp _ [x, y]) t -> Just (evByFiat "regex-types" x y)
    _                              -> Nothing

evRegexEquality :: Ct -> Maybe EvTerm
evRegexEquality ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred _ x y -> Just (evByFiat "regex-types" x y)
    _            -> Nothing

--
-- A Regex a is subsumed by the Regex b if a|b represents the same as a
-- e.g. aa is contained in a+ as a+ == (aa)|(a+)
--
regexContains :: Type -> Type -> Maybe Bool
regexContains (LitTy (StrTyLit fsx)) (LitTy (StrTyLit fsy))
  = let sx = unpackFS fsx
        sy = unpackFS fsy
        rsx = "(" ++ sx ++ ")|(" ++ sy ++ ")"
        rsy = "(" ++ sy ++ ")"
        hsx = H.regExp2Dfa <$> H.parseRegExp rsx
        hsy = H.regExp2Dfa <$> H.parseRegExp rsy
    in liftA2 H.equivDfa hsx hsy
regexContains _ _ = Nothing

regexIntersects :: TyCon -> Type -> Type -> Maybe Bool
regexIntersects tc x y = regexIntersects' $ reduceRegexIntersects tc x y

regexIntersects' :: Maybe (H.Dfa Int Char) -> Maybe Bool
regexIntersects' hs = let e = H.beautifyDfa . H.regExp2Dfa <$> H.parseRegExp ""
                      in not <$> liftA2 H.equivDfa e hs

reduceRegexIntersects :: TyCon -> Type -> Type -> Maybe (H.Dfa Int Char)
reduceRegexIntersects _ (LitTy (StrTyLit fsx)) (LitTy (StrTyLit fsy))
  = let sx = unpackFS fsx
        sy = unpackFS fsy
        hsx = H.regExp2Dfa <$> H.parseRegExp sx
        hsy = H.regExp2Dfa <$> H.parseRegExp sy
    in H.beautifyDfa <$> regexIntersection hsx hsy
reduceRegexIntersects rixtc (TyConApp tc [a,b]) (LitTy (StrTyLit fsy))
  | tc == rixtc = let hsx = reduceRegexIntersects rixtc a b 
                      sy  = unpackFS fsy
                      hsy = H.beautifyDfa . H.regExp2Dfa <$> H.parseRegExp sy
                  in H.beautifyDfa <$> regexIntersection hsx hsy
  | otherwise   = Nothing

regexIntersection :: (Ord a, Eq b) => Maybe (H.Dfa a b) -> Maybe (H.Dfa a b) -> Maybe (H.Dfa [a] b)
regexIntersection hsx hsy = H.ndfa2dfa <$> liftA2 intersectDfa hsx hsy

intersectDfa :: (Eq a, Eq b) => H.Dfa a b -> H.Dfa a b -> H.Ndfa a b
intersectDfa (H.Dfa vp qp sp zp dp) (H.Dfa vq qq sq zq dq) = H.Ndfa v' q' s' z' d'
  where
    v'           = vp `intersect` vq
    q'           = qp `intersect` qq
    s'           = [sp, sq]
    z'           = zp ++ zq
    d' _ Nothing = []
    d' q (Just sy)
      | q `elem` qp && sy `elem` vp = [dp q sy]
      | q `elem` qq && sy `elem` vq = [dq q sy]
      | otherwise                   = []

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
      let kt = (mkTyConApp key [t])
      ev <- newWanted (ctLoc ct) kt
      return $ CTyEqCan ev v kt NomEq

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
    traceInit = do
      tcPluginDebug ("tcPluginInit " ++ s) empty >> tcPluginInit

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

--
-- @file
--
-- @brief Extra functions for working with GHC
--
-- lookupModule, lookupName, evByFiat, tracePlugin
-- inlined from https://github.com/clash-lang/ghc-tcplugins-extra/blob/master/src/GHC/TcPluginM/Extra.hs
-- to avoid an extra dependency
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE CPP, RecordWildCards #-}

module Elektra.GhcPluginExtras (debugS, debugType, debug, tcPluginDebug, lookupModule, tracePlugin, evByFiat) where

-- GHC API
import GhcPlugins
import TcPluginM  (TcPluginM, tcPluginIO, findImportedModule)
import Outputable (Outputable (..), (<+>), text, ppr)
import TcEvidence (EvTerm (..))
import TyCoRep    (Type(..), UnivCoProvenance (..))
import Panic      (panicDoc)
import TcRnTypes  (TcPlugin(..), TcPluginResult(..))

-- Some debugging utilities
debugS :: String -> TcPluginM ()
debugS = tcPluginIO . putStrLn

debugType :: Type -> TcPluginM ()
debugType = debugS . showSDocUnsafe . pprType

debug :: SDoc -> TcPluginM ()
debug = debugS . showSDocUnsafe

tcPluginDebug :: String -> SDoc -> TcPluginM ()
tcPluginDebug a b = debug $ text a <+> b

lookupModule :: ModuleName -- ^ Name of the module
             -> FastString -- ^ Name of the package containing the module
             -> TcPluginM Module
lookupModule mod_nm pkg = do
  found_module <- findImportedModule mod_nm $ Just pkg
  case found_module of
    Found _ h -> return h
    _             -> do
      found_module' <- findImportedModule mod_nm $ Just $ fsLit "this"
      case found_module' of
        Found _ h -> return h
        _             -> panicDoc "Unable to resolve module looked up by plugin: " (ppr mod_nm)

evByFiat :: String -- ^ Name the coercion should have
         -> Type   -- ^ The LHS of the equivalence relation (~)
         -> Type   -- ^ The RHS of the equivalence relation (~)
         -> EvTerm
evByFiat name t1 t2 =
#if MIN_VERSION_ghc(8,5,0)
    EvExpr $ Coercion
#else
    EvCoercion
#endif
      $ mkUnivCo (PluginProv name) Nominal t1 t2

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

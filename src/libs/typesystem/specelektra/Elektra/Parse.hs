--
-- @file
--
-- @brief Parses GHC's internal type representation to an intermediate data structure and vice versa
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
--
module Elektra.Parse (fromAST, toAST) where

import Data.Maybe    (fromMaybe)
import Control.Monad (mapM, join)

import Elektra.Types
import qualified FiniteAutomata as FA

import GhcPlugins
import TyCoRep (Type(..), TyLit(..))

-- fromAST ensures that every regex expression is a valid regex otherwise it returns EmptyRegex
fromAST :: Type -> IO RegexType
fromAST (TyConApp _ [l, r]) = do
  l' <- fromAST l
  r' <- fromAST r
  return $ if l' == EmptyRegex || r' == EmptyRegex then EmptyRegex else RegexIntersection l' r'
fromAST (LitTy (StrTyLit s)) = let r = (unpackFS s) in fromMaybe EmptyRegex <$> fmap (Regex r) . rightToMaybe <$> FA.compile r
fromAST (TyVarTy v) = return $ RegexVar v
fromAST _ = return EmptyRegex

toAST :: TyCon -> RegexType -> Type
toAST tc (RegexIntersection l r) = TyConApp tc [toAST tc l, toAST tc r]
toAST _ (Regex r _)              = LitTy . StrTyLit $ mkFastString r
toAST _ (RegexVar v)             = TyVarTy v
toAST _ EmptyRegex               = error "toAST called EmptyRegex"

--
-- @file
--
-- @brief Parses GHC's internal type reprentation to an intermediate data structure and vice versa
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

-- This also ensures that every regex expression is a valid regex otherwise it returns EmptyRegex
fromAST :: Type -> IO RegexType
fromAST (TyConApp _ [l, r]) = do
  l' <- fromAST l
  r' <- fromAST r
  return $ if l' == EmptyRegex || r' == EmptyRegex then EmptyRegex else RegexIntersection l' r'
-- Normalize the regex representation here using FA minimization
fromAST (LitTy (StrTyLit s)) = fromMaybe EmptyRegex <$> normalizeRegex (unpackFS s)
  where
    normalizeRegex :: String -> IO (Maybe RegexType)
    normalizeRegex r = do
      n <- rightToMaybe <$> FA.compile r
      mapM_ FA.minimize n
      n' <- join <$> mapM (fmap rightToMaybe . FA.asRegexp) n
      return $ Regex <$> n' <*> n

fromAST (TyVarTy v) = return $ RegexVar v
fromAST _ = return EmptyRegex

toAST :: TyCon -> RegexType -> Type
toAST tc (RegexIntersection l r) = TyConApp tc [toAST tc l, toAST tc r]
toAST _ (Regex r _)              = LitTy . StrTyLit $ mkFastString r
toAST _ (RegexVar v)             = TyVarTy v
toAST _ EmptyRegex               = error "toAST called EmptyRegex"

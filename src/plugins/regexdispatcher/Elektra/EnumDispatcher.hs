--
-- @file
--
-- @brief Regex generation for enums
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TupleSections #-}

module Elektra.EnumDispatcher (enumDispatch) where

import Elektra.Key
import Elektra.KeySet

import Elektra.Dispatch
import Elektra.Range
import Elektra.Parsers

import Data.List (intercalate, permutations)
import Control.Monad (mapM)
import Data.Maybe (catMaybes)

enumDispatch :: Dispatcher
enumDispatch ks = ksList ks >>= fmap catMaybes . mapM dispatch
  where
    dispatch k = do
      es <- filterMeta "check/enum/#" k
      if null es then return Nothing else 
        keyGetMeta k "check/enum/multi" >>=
        ifKey (singleEnum es) (multiEnum es) >>=
        return . Just . (k, "check/validation", )
    multiEnum es s = do
      s <- keyString s
      let group = ('(' :) . (++ ")") . intercalate s
      let genUniqueCombinations = intercalate "|" . map group . permutations
      fmap genUniqueCombinations . mapM keyString $ es
    singleEnum = fmap (intercalate "|") . mapM keyString

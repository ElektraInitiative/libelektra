--
-- @file
--
-- @brief Regex generation for numerical ranges
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TupleSections #-}

module Elektra.RangeDispatcher (rangeDispatch) where

import Elektra.Key
import Elektra.KeySet

import Elektra.Dispatch
import Elektra.Range
import Elektra.Parsers

import Control.Monad (filterM, (>=>), mapM)
import Data.Maybe (catMaybes)

rangeDispatch :: Dispatcher
rangeDispatch ks = ksList ks >>= fmap catMaybes . mapM dispatch
  where
  	dispatch k    = ifKey (keyGetMeta k "check/range") (dispatch' k) (return Nothing) 
  	dispatch' k m = fmap (k, dispatchedPrefix ++ "check/range", ) . fmap (uncurry regexForRange) . parseRange <$> keyString m

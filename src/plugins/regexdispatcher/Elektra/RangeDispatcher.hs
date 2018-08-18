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
    dispatch k    = keyGetMeta k "check/range" >>= ifKey (return Nothing) (dispatch' k)
    dispatch' k m = fmap ((k, "check/validation", ) . uncurry regexForRange) . parseRange <$> keyString m

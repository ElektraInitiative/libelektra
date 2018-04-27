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
rangeDispatch ks = selector ks >>= fmap catMaybes . mapM generator 
  where
  	selector = ksList >=> filterM (filterMeta "check/range")
  	generator k = do
  		cr <- keyGetMeta k "check/range"
  		name <- ("elektra/spec/regex/" ++) <$> keyName cr
  		fmap (k, name, ) . fmap (uncurry regexForRange) . parseRange <$> keyString cr

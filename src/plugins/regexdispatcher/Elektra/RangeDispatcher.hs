--
-- @file
--
-- @brief Regex generation for numerical ranges
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.RangeDispatcher (rangeDispatch) where

import Elektra.Key
import Elektra.KeySet

import Elektra.Dispatch
import Elektra.Range
import Elektra.Parsers

import Control.Monad (filterM, (>=>), (<=<), join)
import Data.Maybe (catMaybes)

rangeDispatch :: Dispatcher
rangeDispatch ks = selector ks >>= fmap catMaybes . mapM generator 
  where
  	selector = ksList >=> filterM (filterMeta "check/range") >=> mapM keyString
  	generator = sequence . fmap keyNew . fmap (uncurry regexForRange) . parseRange

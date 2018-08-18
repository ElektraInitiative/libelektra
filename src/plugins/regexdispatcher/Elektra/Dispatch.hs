--
-- @file
--
-- @brief General definitions of a dispatcher and some utility functions
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 

module Elektra.Dispatch (Dispatcher, GeneratedRegex, filterMeta) where

import Elektra.Key
import Elektra.KeySet

import Data.List (isPrefixOf)
import Control.Monad (filterM)

type GeneratedRegex = (Key, String, String)
type Dispatcher = KeySet -> IO [GeneratedRegex]

filterMeta :: String -> Key -> IO [Key]
filterMeta s k = keyListMeta k >>= filterM (fmap (isPrefixOf s) . keyName)

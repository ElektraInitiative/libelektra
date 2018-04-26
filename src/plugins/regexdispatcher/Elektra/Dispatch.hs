--
-- @file
--
-- @brief General definitions of a dispatcher
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 

module Elektra.Dispatch (Dispatcher, filterMeta) where

import Elektra.Key
import Elektra.KeySet

type Dispatcher = KeySet -> IO [Key]

filterMeta :: String -> Key -> IO Bool
filterMeta s k = ifKey (keyGetMeta k s) (const $ return True) (return False)

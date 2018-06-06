--
-- @file
--
-- @brief Generates regex-representations of specification keywords
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE OverloadedStrings #-}

module Elektra.RegexDispatcher () where

import Elektra.Key
import Elektra.KeySet
import Elektra.Ease
import Elektra.Plugin

import Elektra.Dispatch
import Elektra.RangeDispatcher
import Elektra.EnumDispatcher
import Elektra.ValidationDispatcher

import Foreign.Ptr
import Data.Bool
import Control.Monad (sequence, forM_, mapM_, (>=>))

import qualified Data.Text as T

dispatch :: KeySet -> Key -> IO Bool
dispatch ks k = do
  dispatched <- fmap concat . sequence $ map ($ ks) [rangeDispatch, enumDispatch]
  forM_ dispatched . uncurry $ flip keySetMeta "check/validation"
  return . not $ null dispatched

elektraRegexdispatcherOpen :: Plugin -> Key -> IO PluginStatus
elektraRegexdispatcherOpen p k = keySetMeta k "/plugins/regexdispatcher" "elektraRegexdispatcherOpen" >> return Success
hs_elektraHaskellOpen = elektraPluginOpenWith elektraRegexdispatcherOpen

elektraRegexdispatcherClose :: Plugin -> Key -> IO PluginStatus
elektraRegexdispatcherClose p k = keySetMeta k "/plugins/regexdispatcher" "elektraRegexdispatcherClose" >> return Success
hs_elektraHaskellClose = elektraPluginCloseWith elektraRegexdispatcherClose

elektraRegexdispatcherGet :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraRegexdispatcherGet p ks k = keySetMeta k "/plugins/regexdispatcher" "elektraRegexdispatcherGet" >> return NoUpdate
hs_elektraHaskellGet = elektraPluginGetWith elektraRegexdispatcherGet

elektraRegexdispatcherSet :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraRegexdispatcherSet p ks k = bool NoUpdate Success <$> dispatch ks k 
hs_elektraHaskellSet = elektraPluginSetWith elektraRegexdispatcherSet

elektraRegexdispatcherError :: Plugin -> KeySet -> Key -> IO PluginStatus
elektraRegexdispatcherError p ks k = keySetMeta k "/plugins/regexdispatcher" "elektraRegexdispatcherError" >> return Success
hs_elektraHaskellError = elektraPluginErrorWith elektraRegexdispatcherError

foreign export ccall hs_elektraHaskellOpen :: Ptr Plugin -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellClose :: Ptr Plugin -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellGet :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellSet :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int
foreign export ccall hs_elektraHaskellError :: Ptr Plugin -> Ptr KeySet -> Ptr Key -> IO Int

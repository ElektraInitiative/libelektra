--
-- @file
--
-- @brief Regex generation for numerical ranges
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TupleSections #-}

module Elektra.DefaultDispatcher (defaultDispatch) where

import Data.List (concatMap, elem)

import Elektra.Key
import Elektra.KeySet

import Elektra.Dispatch

import Control.Monad (filterM, mapM)
import Data.Maybe (catMaybes)

defaultDispatch :: Dispatcher
defaultDispatch ks = ksList ks >>= fmap catMaybes . mapM dispatch
  where
    dispatch k    = keyGetMeta k "default" >>= ifKey (return Nothing) (dispatch' k)
    dispatch' k m = Just . (k, "defaultValue", ) . concatMap escapeRegexChar <$> keyString m
    -- escape regex keywords
    regexKeywords = ".\\+*?[^]$(){}=!<>|:-"
    escapeRegexChar x = if x `elem` regexKeywords then '\\' : [x] else [x]

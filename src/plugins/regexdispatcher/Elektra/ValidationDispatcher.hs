--
-- @file
--
-- @brief Regex generation for regexes ;)
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
{-# LANGUAGE TupleSections #-}

module Elektra.ValidationDispatcher (validationDispatch) where

import Elektra.Key
import Elektra.KeySet

import Elektra.Dispatch
import Elektra.Range
import Elektra.Parsers

import FiniteAutomata

import Control.Monad (filterM, (>=>), mapM, when)
import Data.Maybe (catMaybes, isJust)

validationDispatch :: Dispatcher
validationDispatch ks = ksList ks >>= fmap catMaybes . mapM dispatch
  where
    dispatch k    = keyGetMeta k "check/validation" >>= ifKey (return Nothing) (dispatch' k)
    dispatch' k m = do
        rgx <- keyString m
        compiledRgx <- either (const Nothing) Just <$> compile rgx
        fmap (k, ) <$> processAdditionalMetakeys compiledRgx
      where
        processAdditionalMetakeys Nothing = return Nothing
        processAdditionalMetakeys (Just rgx) = do
          complementedRgx <- keyGetMeta k "check/validation/invert" >>= ifKey (return rgx) (const $ complement rgx)
          keyGetMeta k "check/validation/ignorecase" >>= whenKey (const $ const () <$> nocase complementedRgx)
          _ <- minimize complementedRgx
          either (const Nothing) Just <$> asRegexp complementedRgx

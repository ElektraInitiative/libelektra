--
-- @file
--
-- @brief Haskell bindings for libelektra-invoke
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Invoke (
  InvokeFunction,
  elektraInvokeOpen, elektraInvokeGetFunction, 
  elektraInvokeGetPluginConfig, elektraInvokeGetPluginName, elektraInvokeGetPluginData, elektraInvokeGetModules,
  elektraInvokeGetExports, elektraInvoke2Args, elektraInvokeClose,
  ifHandle
) where

{#import Elektra.Key #}
{#import Elektra.KeySet #}
{#import Elektra.Plugin #}

import Data.Maybe (maybe)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.ForeignPtr (withForeignPtr, newForeignPtr_)

#include <kdbinvoke.h>

{#pointer *ElektraInvokeHandle newtype #}
type InvokeFunction = Ptr ()

-- Handles are already pointers, so just cast them from/to void pointers
instance PluginData ElektraInvokeHandle where
  store a (ElektraInvokeHandle p) = a $ castPtr p
  retrieve p = if p == nullPtr then Nothing else (Just . ElektraInvokeHandle $ castPtr p)

ifHandle :: IO a -> (ElektraInvokeHandle -> IO a) -> ElektraInvokeHandle -> IO a
ifHandle f t h@(ElektraInvokeHandle p) = if p == nullPtr then f else t h

-- ***
-- Invoke METHODS
-- ***

elektraInvokeOpen :: String -> Maybe KeySet -> Maybe Key -> IO ElektraInvokeHandle
elektraInvokeOpen elektraPluginName config errorKey = do
  c <- maybe (ksNew 0) return config
  e <- maybe (keyNew "") return errorKey
  elektraInvokeOpenRaw elektraPluginName c e
{#fun unsafe elektraInvokeOpen as elektraInvokeOpenRaw {`String', `KeySet', `Key'} -> `ElektraInvokeHandle' #}

{#fun unsafe elektraInvokeGetFunction {`ElektraInvokeHandle', `String'} -> `InvokeFunction' return* #}
{#fun unsafe elektraInvokeGetPluginConfig {`ElektraInvokeHandle'} -> `KeySet' #}
{#fun unsafe elektraInvokeGetPluginName {`ElektraInvokeHandle'} -> `String' #}
elektraInvokeGetPluginData :: PluginData d => ElektraInvokeHandle -> IO (Maybe d)
elektraInvokeGetPluginData h = retrieve <$> elektraInvokeGetPluginDataRaw h
{#fun unsafe elektraInvokeGetPluginData as elektraInvokeGetPluginDataRaw {`ElektraInvokeHandle'} -> `Ptr ()' return* #}
{#fun unsafe elektraInvokeGetModules {`ElektraInvokeHandle'} -> `KeySet' #}
{#fun unsafe elektraInvokeGetExports {`ElektraInvokeHandle'} -> `KeySet' #}
{#fun unsafe elektraInvoke2Args {`ElektraInvokeHandle', `String', `KeySet', `Key'} -> `Int' #}

elektraInvokeClose :: ElektraInvokeHandle -> Maybe Key -> IO ()
elektraInvokeClose handle errorKey = do
  e <- maybe (keyNew "") return errorKey
  elektraInvokeCloseRaw handle e
{#fun unsafe elektraInvokeClose as elektraInvokeCloseRaw {`ElektraInvokeHandle',`Key'} -> `()' #}

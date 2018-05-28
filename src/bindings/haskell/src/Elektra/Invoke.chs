--
-- @file
--
-- @brief invoke Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Invoke (
  InvokeFunction, PluginData,
  elektraInvokeOpen, elektraInvokeGetFunction, 
  elektraInvokeGetPluginConfig, elektraInvokeGetPluginName, elektraInvokeGetPluginData, elektraInvokeGetModules,
  elektraInvokeGetExports, elektraInvoke2Args, elektraInvokeClose
) where

{#import Elektra.Key#}
{#import Elektra.KeySet#}

import Data.Maybe (maybe)
import Foreign.Ptr (castPtr, nullPtr)

#include <kdbinvoke.h>

{#pointer *ElektraInvokeHandle foreign newtype #}
type InvokeFunction = C2HSImp.Ptr ()
type PluginData = C2HSImp.Ptr ()

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
{#fun unsafe elektraInvokeGetPluginData {`ElektraInvokeHandle'} -> `PluginData' return* #}
{#fun unsafe elektraInvokeGetModules {`ElektraInvokeHandle'} -> `KeySet' #}
{#fun unsafe elektraInvokeGetExports {`ElektraInvokeHandle'} -> `KeySet' #}
{#fun unsafe elektraInvoke2Args {`ElektraInvokeHandle', `String', `KeySet', `Key'} -> `Int' #}

elektraInvokeClose :: ElektraInvokeHandle -> Maybe Key -> IO ()
elektraInvokeClose handle errorKey = do
  e <- maybe (keyNew "") return errorKey
  elektraInvokeCloseRaw handle e
{#fun unsafe elektraInvokeClose as elektraInvokeCloseRaw {`ElektraInvokeHandle',`Key'} -> `()' #}

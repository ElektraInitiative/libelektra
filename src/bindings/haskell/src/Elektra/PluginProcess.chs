--
-- @file
--
-- @brief PluginProcess Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.PluginProcess (
  ElektraPluginProcess,
  elektraPluginProcessGetData,
  elektraPluginProcessSetData
) where

{#import Elektra.Plugin#}
{#import Elektra.Invoke#}

import Foreign.Ptr (Ptr, castPtr, nullPtr)

#include <kdbpluginprocess.h>

{#pointer *ElektraPluginProcess newtype #}

-- Handles are already pointers, so just cast them from/to void pointers
instance PluginData ElektraPluginProcess where
  store a (ElektraPluginProcess p) = a $ castPtr p
  retrieve p = if p == nullPtr then Nothing else (Just . ElektraPluginProcess $ castPtr p)

-- ***
-- PLUGINPROCESS METHODS
-- ***

elektraPluginProcessSetData :: PluginData d => ElektraPluginProcess -> d -> IO ()
elektraPluginProcessSetData p = store (elektraPluginProcessSetDataRaw p)
{#fun unsafe elektraPluginProcessSetData as elektraPluginProcessSetDataRaw {`ElektraPluginProcess', `Ptr ()'} -> `()' #}

elektraPluginProcessGetData :: PluginData d => ElektraPluginProcess -> IO (Maybe d)
elektraPluginProcessGetData p = retrieve <$> elektraPluginProcessGetDataRaw p
{#fun unsafe elektraPluginProcessGetData as elektraPluginProcessGetDataRaw {`ElektraPluginProcess'} -> `Ptr ()' #}

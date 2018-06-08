--
-- @file
--
-- @brief PluginProcess Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.PluginProcess (
  elektraPluginProcessGetData,
  elektraPluginProcessSetData
) where

{#import Elektra.Plugin#}
{#import Elektra.Invoke#}

import Foreign.Ptr (Ptr)

#include <kdbpluginprocess.h>

-- ***
-- PLUGINPROCESS METHODS
-- ***

elektraPluginProcessSetData :: PluginData d => Plugin -> d -> IO ()
elektraPluginProcessSetData p = store (elektraPluginProcessSetDataRaw p)
{#fun unsafe elektraPluginProcessSetData as elektraPluginProcessSetDataRaw {`Plugin', `Ptr ()'} -> `()' #}

elektraPluginProcessGetData :: PluginData d => Plugin -> IO (Maybe d)
elektraPluginProcessGetData p = retrieve <$> elektraPluginProcessGetDataRaw p
{#fun unsafe elektraPluginProcessGetData as elektraPluginProcessGetDataRaw {`Plugin'} -> `Ptr ()' #}

module Elektra.Plugin (elektraPluginGetConfig, elektraPluginSetData, elektraPluginGetData) where

{#import Elektra.Key#}
{#import Elektra.KeySet#}

#include <kdbplugin.h>

{#pointer *Plugin foreign newtype #}

-- ***
-- PLUGIN METHODS
-- ***

{#fun unsafe elektraPluginGetConfig {`Plugin'} -> `KeySet' #}
-- You have to cast it using Foreign.Ptr.castPtr to the data structure you use manually
{#fun unsafe elektraPluginSetData {`Plugin', `Ptr ()'} -> `()' #}
{#fun unsafe elektraPluginGetData {`Plugin'} -> `Ptr ()' #}

-- foreign export ccall elektraPluginOpen :: Plugin -> Key -> Int

-- TODO provide the plugin methods as a type class for the different plugins along with a convenient way to register them?
-- IDEA: We still need a c file too, which initializes the haskell environment and asks haskell for the function pointers for our plugin
-- Then in the c file it calls startupHaskell and shutdownHaskell, the rest gets either directly forwarded to the haskell functions or pulled through
-- {#fun unsafe elektraPlugin {`KDB', `KeySet', `Key'} -> `Int' #}

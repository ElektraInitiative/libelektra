module Elektra.Plugin where

{#import Elektra.Key#}
{#import Elektra.KeySet#}

#include <kdbplugin.h>

{#pointer *Plugin foreign newtype #}

-- ***
-- PLUGIN METHODS
-- ***

-- TODO this calls back into haskell, show how we could handle function pointers here
--{#fun elektraPluginExport {`Key'} -> `KDB' #}
--{#fun unsafe elektraPluginGetConfig {`Plugin'} -> `KeySet' #}
--{#fun unsafe elektraPluginSetData {`Plugin',} -> `Int' #}
--{#fun unsafe elektraPluginGetData {`KDB', `KeySet', `Key'} -> `Int' #}

-- TODO provide the plugin methods as a type class for the different plugins along with a convenient way to register them?
-- IDEA: We still need a c file too, which initializes the haskell environment and asks haskell for the function pointers for our plugin
-- Then in the c file it calls startupHaskell and shutdownHaskell, the rest gets either directly forwarded to the haskell functions or pulled through
-- {#fun unsafe elektraPlugin {`KDB', `KeySet', `Key'} -> `Int' #}

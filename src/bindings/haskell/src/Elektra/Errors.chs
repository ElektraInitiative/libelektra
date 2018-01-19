--
-- @file
--
-- @brief Errors Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Errors (
  triggerError, triggerWarnings
) where

#include <hs_kdberrors.h>

{#import Elektra.Key#}

{#context lib="libelektra" #}

{#fun unsafe hs_elektraTriggerError as triggerError {`Int', `Key', `String'} -> `()' #}
{#fun unsafe hs_elektraTriggerWarnings as triggerWarnings {`Int', `Key', `String'} -> `()' #}

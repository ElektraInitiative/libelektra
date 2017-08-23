module Elektra.KeySet (KeySet (..), withKeySet, ElektraCursor, ksNew, ksDup, ksCopy, ksDel, ksGetSize, ksNeedSync,
    ksAppendKey, ksAppend, ksCut, ksPop, ksRewind, ksNext, ksCurrent, ksHead, ksTail,
    ksGetCursor, ksSetCursor, ksAtCursor, ksLookup, ksLookupByName) where

{#import Elektra.Key#}

#include <kdb.h>

{#pointer *KeySet foreign finalizer ksDel newtype #}

-- TODO consider making this a newtype to prevent abusal?
type ElektraCursor = Int
{#typedef cursor_t ElektraCursor #}
{#default in `ElektraCursor' [cursor_t] fromIntegral #}
{#default out `ElektraCursor' [cursor_t] fromIntegral #}

-- ***
-- KEYSET CREATION / DELETION / COPY METHODS
-- ***

-- To get around the variadic arguments, we simply directly pass 0 to denote the nullptr for now
-- varargs are not supported by haskell, so apart from defining some fixed amounts of key parameters
-- there is not much more we can do
ksNew size = ksNewTerminated size 0
{#fun unsafe variadic ksNew[int] as ksNewTerminated {`Int', `Int'} -> `KeySet' #}
{#fun unsafe ksDup {`KeySet'} -> `KeySet' #}
{#fun unsafe ksCopy {`KeySet', `KeySet'} -> `Int' #}

{#fun unsafe ksGetSize {`KeySet'} -> `Int' #}
{#fun unsafe ksNeedSync {`KeySet'} -> `Int' #}

-- ***
-- KEYSET MODIFICATION
-- ***

{#fun unsafe ksAppendKey {`KeySet', `Key'} -> `Int' #}
{#fun unsafe ksAppend {`KeySet', `KeySet'} -> `Int' #}
{#fun unsafe ksCut {`KeySet', `Key'} -> `KeySet' #}
{#fun unsafe ksPop {`KeySet'} -> `Key' #}

-- ***
-- KEYSET LOOKUP/ITERATION
-- ***

{#fun unsafe ksRewind {`KeySet'} -> `Int' #}
{#fun unsafe ksNext {`KeySet'} -> `Key' #}
{#fun unsafe ksCurrent {`KeySet'} -> `Key' #}
{#fun unsafe ksHead {`KeySet'} -> `Key' #}
{#fun unsafe ksTail {`KeySet'} -> `Key' #}
{#fun unsafe ksGetCursor {`KeySet'} -> `ElektraCursor' #}
{#fun unsafe ksSetCursor {`KeySet', `ElektraCursor'} -> `Int' #}
{#fun unsafe ksAtCursor {`KeySet', `ElektraCursor'} -> `Key' #}
{#fun unsafe ksLookup {`KeySet', `Key', `LookupOptions'} -> `Key' #}
{#fun unsafe ksLookupByName {`KeySet', `String', `LookupOptions'} -> `Key' #}

module Elektra.Key (Key (..), Namespace (..), LookupOptions (..), withKey,
    keyNew, keyDup, keyCopy, keyClear, keyIncRef, keyDecRef, keyGetRef,
    keyName, keyGetNameSize, keyGetUnescapedNameSize, keySetName, keyGetFullNameSize, keyAddName, 
    keyBaseName, keyGetBaseNameSize, keyAddBaseName, keySetBaseName, keyGetNamespace,
    keyString, keyGetValueSize, keySetString,
    keyRewindMeta, keyNextMeta, keyCurrentMeta, keyCopyMeta, keyCopyAllMeta, keyGetMeta, keySetMeta,
    keyCmp, keyNeedSync, keyIsBelow, keyIsDirectBelow, keyRel, keyIsInactive, keyIsBinary, keyIsString) where

#include <kdb.h>

{#context lib="libelektra" #}

-- ***
-- TYPE DEFINITIONS
-- ***

{#pointer *Key foreign finalizer keyDel newtype #}

{#typedef size_t Int#}
{#typedef ssize_t Int #}

-- TODO put in own shared file and hide the internal FFI constructors

{#enum KEY_NS_NONE as Namespace { underscoreToCase } deriving (Show, Eq) #}
--{#enum KEY_NAME as ElektraKeyVarargs { underscoreToCase } deriving (Show, Eq) #} not required, we don't use the varargs version
{#enum KDB_O_NONE as LookupOptions { underscoreToCase } deriving (Show, Eq) #}

-- ***
-- KEY CREATION / DELETION / COPPY METHODS
-- Note there is no varargs in haskell directly, hacked via lists
-- ***

{#fun unsafe keyNew {`String'} -> `Key' #}
{#fun unsafe keyDup {`Key'} -> `Key' #}
{#fun unsafe keyCopy {`Key', `Key'} -> `Int' #}
{#fun unsafe keyClear {`Key'} -> `Int' #}
{#fun unsafe keyIncRef {`Key'} -> `Int' #}
{#fun unsafe keyDecRef {`Key'} -> `Int' #}
{#fun unsafe keyGetRef {`Key'} -> `Int' #}

-- ***
-- KEY NAME MANIPULATION METHODS
-- ***

{#fun unsafe keyName {`Key'} -> `String' #}
{#fun unsafe keyGetNameSize {`Key'} -> `Int' #}
--{#fun keyUnescapedName {`Key'} -> `String' #}
{#fun unsafe keyGetUnescapedNameSize {`Key'} -> `Int' #}
-- TODO leave those away, as it probably makes very little sense to have these in a haskell binding
--{#fun unsafe keyGetName {`Key', alloca- `String' peekCString*, `Int'} -> `Int'#}
{#fun unsafe keySetName {`Key', `String'} -> `Int' #}
{#fun unsafe keyGetFullNameSize {`Key'} -> `Int' #}
--{#fun keyGetFullName {`Key'} -> `String' #}
{#fun unsafe keyBaseName {`Key'} -> `String' #}
{#fun unsafe keyGetBaseNameSize {`Key'} -> `Int' #}
--{#fun keyGetBaseName {`Key'} -> `String' #}
{#fun unsafe keyAddBaseName {`Key', `String'} -> `Int' #}
{#fun unsafe keyAddName {`Key', `String'} -> `Int' #}
{#fun unsafe keySetBaseName {`Key', `String'} -> `Int' #}
{#fun unsafe keyGetNamespace {`Key'} -> `Namespace' #}

-- ***
-- KEY VALUE MANIPULATION METHODS
-- ***

-- we don't handle binary data currently
-- {#fun unsafe keyValue {`Key'} -> `PPtr' #}
{#fun unsafe keyString {`Key'} -> `String' #}
{#fun unsafe keyGetValueSize {`Key'} -> `Int' #}
{#fun unsafe keySetString {`Key', `String'} -> `Int' #}

-- ***
-- KEY META MANIPULATION METHODS
-- ***

{#fun unsafe keyRewindMeta {`Key'} -> `Int' #}
{#fun unsafe keyNextMeta {`Key'} -> `Key' #}
{#fun unsafe keyCurrentMeta {`Key'} -> `Key' #}
{#fun unsafe keyCopyMeta {`Key', `Key', `String'} -> `Int' #}
{#fun unsafe keyCopyAllMeta {`Key', `Key'} -> `Int' #}
{#fun unsafe keyGetMeta {`Key', `String'} -> `Key' #}
{#fun unsafe keySetMeta {`Key', `String', `String'} -> `Int' #}

-- ***
-- KEY TESTING METHODS
-- ***

{#fun unsafe keyCmp {`Key', `Key'} -> `Int' #}
{#fun unsafe keyNeedSync {`Key'} -> `Int' #}
{#fun unsafe keyIsBelow {`Key', `Key'} -> `Int' #}
{#fun unsafe keyIsDirectBelow {`Key', `Key'} -> `Int' #}
{#fun unsafe keyRel {`Key', `Key'} -> `Int' #}
{#fun unsafe keyIsInactive {`Key'} -> `Int' #}
{#fun unsafe keyIsBinary {`Key'} -> `Int' #}
{#fun unsafe keyIsString {`Key'} -> `Int' #}
-- testing out custom marshallers, this probably fits better to haskell
{#fun unsafe keyIsString as keyIsStringMaybe {`Key'} -> `Maybe Bool' peekMaybeBool #}

-- ***
-- CUSTOM MARSHALLERS WHICH FIT HASKELL BETTER
-- ***

withMaybeBool (Just False) = 0
withMaybeBool (Just True) = 1
withMaybeBool Nothing = -1 

peekMaybeBool x =
    case fromIntegral x of
        -1 -> Nothing
        0 -> Just False
        _ -> Just True

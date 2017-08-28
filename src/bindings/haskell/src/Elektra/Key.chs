module Elektra.Key (Key (..), Namespace (..), LookupOptions (..), withKey,
    keyNew, keyDup, keyCopy, keyClear, keyIncRef, keyDecRef, keyGetRef,
    keyName, keyGetNameSize, keyUnescapedName, keyGetUnescapedNameSize, keySetName, keyGetFullNameSize, keyGetFullName,
    keyAddName,  keyBaseName, keyGetBaseNameSize, keyAddBaseName, keySetBaseName, keyGetNamespace,
    keyString, keyGetValueSize, keySetString,
    keyRewindMeta, keyNextMeta, keyCurrentMeta, keyCopyMeta, keyCopyAllMeta, keyGetMeta, keySetMeta,
    keyCmp, keyNeedSync, keyIsBelow, keyIsDirectBelow, keyRel, keyIsInactive, keyIsBinary, keyIsString) where

#include <kdb.h>
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)

{#context lib="libelektra" #}

-- ***
-- TYPE DEFINITIONS
-- ***

{#pointer *Key foreign finalizer keyDel newtype #}

{#typedef size_t Int#}
{#typedef ssize_t Int #}

{#enum KEY_NS_NONE as Namespace { underscoreToCase } deriving (Show, Eq) #}
--{#enum KEY_NAME as ElektraKeyVarargs { underscoreToCase } deriving (Show, Eq) #} not required, we don't use the varargs version
{#enum KDB_O_NONE as LookupOptions { underscoreToCase } deriving (Show, Eq) #}

-- ***
-- KEY CREATION / DELETION / COPPY METHODS
-- ***

-- as we use haskell's reference counting here, increase the number by one
-- so it gets deleted properly when haskell calls the finalizer
keyNew name = do
    key <- keyNewRaw name
    keyIncRef key
    return key
{#fun unsafe keyNew as keyNewRaw {`String'} -> `Key' #}
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
keyGetName = keyName
{#fun unsafe keyGetNameSize {`Key'} -> `Int' #}
keyUnescapedName :: (Key) -> IO ((String))
keyUnescapedName key = do
    size <- keyGetUnescapedNameSize key
    withKey key $ (\cKey -> do
        result <- {#call unsafe keyUnescapedName as keyUnescapedNameRaw #} cKey
        C2HSImp.peekCStringLen (castPtr result, size))
{#fun unsafe keyGetUnescapedNameSize {`Key'} -> `Int' #}
{#fun unsafe keySetName {`Key', `String'} -> `Int' #}
keyGetFullName :: (Key) -> IO ((String))
keyGetFullName key = do
    size <- keyGetFullNameSize key
    withKey key $ \cKey -> 
        allocaBytes size (\result -> do
            {#call unsafe keyGetFullName as keyGetFullNameRaw #} cKey result size
            C2HSImp.peekCString result)
{#fun unsafe keyGetFullNameSize {`Key'} -> `Int' #}
{#fun unsafe keyBaseName {`Key'} -> `String' #}
keyGetBaseName = keyBaseName
{#fun unsafe keyGetBaseNameSize {`Key'} -> `Int' #}
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

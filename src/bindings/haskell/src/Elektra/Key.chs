module Elektra.Key (Key (..), Namespace (..), withKey,
    keyNew, keyNewWithValue, keyDup, keyCopy, keyClear, keyIncRef, keyDecRef, keyGetRef,
    keyName, keyGetNameSize, keyUnescapedName, keyGetUnescapedNameSize, keySetName, keyGetFullNameSize, keyGetFullName,
    keyAddName,  keyBaseName, keyGetBaseName, keyGetBaseNameSize, keyAddBaseName, keySetBaseName, keyGetNamespace,
    keyString, keyGetValueSize, keySetString, keySet,
    keyRewindMeta, keyNextMeta, keyCurrentMeta, keyCopyMeta, keyCopyAllMeta, keyGetMeta, keySetMeta, keyListMeta,
    keyCmp, keyNeedSync, keyIsBelow, keyIsDirectBelow, keyRel, keyIsInactive, keyIsBinary, keyIsString, keyPtrNull, ifKey) where

#include <kdb.h>
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.ForeignPtr (withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM)

{#context lib="libelektra" #}

-- ***
-- TYPE DEFINITIONS
-- ***

{#pointer *Key foreign finalizer keyDel newtype #}

{#typedef size_t Int#}
{#typedef ssize_t Int #}

{#enum KEY_NS_NONE as Namespace { underscoreToCase } deriving (Show, Eq) #}
{#enum KEY_NAME as ElektraKeyVarargs { underscoreToCase } deriving (Show, Eq) #}

-- ***
-- KEY CREATION / DELETION / COPPY METHODS
-- ***

keyPtrNull :: Key -> IO Bool
keyPtrNull (Key ptr) = withForeignPtr ptr (return . (== nullPtr))

-- as we use haskell's reference counting here, increase the number by one
-- so it gets deleted properly when haskell calls the finalizer
keyNew :: String -> IO Key
keyNew name = do
    key <- keyNewRaw name KeyEnd
    keyIncRef key
    return key
keyNewWithValue :: String -> String -> IO Key
keyNewWithValue name value = do
    key <- keyNewRawWithValue name KeyValue value KeyEnd
    keyIncRef key
    return key
{#fun unsafe variadic keyNew[keyswitch_t] as keyNewRaw {`String', `ElektraKeyVarargs'} -> `Key' #}
{#fun unsafe variadic keyNew[keyswitch_t, const char *, keyswitch_t]
    as keyNewRawWithValue {`String', `ElektraKeyVarargs', `String', `ElektraKeyVarargs'} -> `Key' #}
keyDup :: Key -> IO Key
keyDup key = do
    dup <- keyDupRaw key
    keyIncRef dup
    return dup
{#fun unsafe keyDup as keyDupRaw {`Key'} -> `Key' #}
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
keyUnescapedName :: Key -> IO String
keyUnescapedName key = do
    size <- keyGetUnescapedNameSize key
    withKey key $ (\cKey -> do
        result <- {#call unsafe keyUnescapedName as keyUnescapedNameRaw #} cKey
        C2HSImp.peekCStringLen (castPtr result, size))
{#fun unsafe keyGetUnescapedNameSize {`Key'} -> `Int' #}
{#fun unsafe keySetName {`Key', `String'} -> `Int' #}
keyGetFullName :: (Key) -> IO String
keyGetFullName key = do
    size <- keyGetFullNameSize key
    withKey key $ \cKey -> 
        allocaBytes size (\result -> do
            {#call unsafe keyGetFullName as keyGetFullNameRaw #} cKey result size
            C2HSImp.peekCString result)
{#fun unsafe keyGetFullNameSize {`Key'} -> `Int' #}
{#fun unsafe keyBaseName {`Key'} -> `String' #}
keyGetBaseName :: Key -> IO String
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
keySet :: Show a => Key -> a -> IO Int
keySet key = keySetString key . show
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
keyListMeta :: Key -> IO [Key]
keyListMeta key = keyRewindMeta key >> listMeta []
    where
        listMeta res = do
            cur <- keyNextMeta key
            isNull <- keyPtrNull cur
            if isNull then return res else liftM (cur :) (listMeta res)

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

-- ***
-- COMMON HASKELL TYPE CLASSES
-- unsafePerformIO should be ok here, as those functions don't alter the key's state
-- ***

instance Show Key where
    show key = unsafePerformIO $ do
        name <- keyName key
        value <- keyString key
        ref <- keyGetRef key
        return $ name ++ " " ++ value ++ " " ++ (show ref)

instance Eq Key where
    key1 == key2 = unsafePerformIO $ fmap (== 0) $ keyCmp key1 key2

-- ***
-- ADDITIONAL HELPERS USEFUL IN HASKELL
-- ***

ifKey :: IO Key -> (Key -> IO a) -> IO a -> IO a
ifKey k t f = do
    null <- k >>= keyPtrNull
    if null then f else k >>= t

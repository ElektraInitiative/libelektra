--
-- @file
--
-- @brief Key Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Key (
  Key (..), Namespace (..), ElektraKeyVarargs (KeyMetaName, KeyBinary, KeyComment, KeyOwner),
  keyNew, keyNewWithValue, keyNewWithFlagsAndValue,
  keyDup, keyCopy, keyClear,
  keyIncRef, keyDecRef, keyGetRef,
  keyName, keyGetNameSize, keySetName, keyAddName, 
  keyUnescapedName, keyGetUnescapedNameSize,
  keyGetFullNameSize, keyGetFullName,
  keyBaseName, keyGetBaseName, keyGetBaseNameSize, keyAddBaseName, keySetBaseName, keyDeleteBaseName,
  keyGetNamespace,
  keyString, keyGetValueSize, keySetString, keySet,
  keyRewindMeta, keyNextMeta, keyCurrentMeta,
  keyCopyMeta, keyCopyAllMeta, keyGetMeta, keySetMeta, keyListMeta,
  keyCmp, keyNeedSync, 
  keyIsBelow, keyIsDirectBelow, 
  keyRel, keyIsInactive, keyIsBinary, keyIsString, keyPtrNull, 
  ifKey, withKey, whenKey,
  tmpRef
) where

#include <elektra/kdb.h>
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.ForeignPtr (FinalizerPtr (..), withForeignPtr, addForeignPtrFinalizer)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (isJust, fromJust)
import Control.Monad (liftM)
import Data.Bool (bool)

{#context lib="libelektra" #}

-- ***
-- TYPE DEFINITIONS
-- ***

{#pointer *Key foreign newtype #}

{#typedef size_t Int#}
{#typedef ssize_t Int #}

{#enum KEY_NS_NONE as Namespace { underscoreToCase } deriving (Show, Eq) #}
{#enum KEY_NAME as ElektraKeyVarargs { underscoreToCase } deriving (Show, Eq) #}

data KeyNew = KeyNew { knKeyName :: Maybe String
                     , knKeyValue :: Maybe String
                     , knMeta :: [(String, String)]
                     , knKeySize :: Maybe Int
                     , knFlags :: [ElektraKeyVarargs]
                     } deriving (Show)

-- ***
-- KEY CREATION / DELETION / COPPY METHODS
-- ***

keyPtrNull :: Key -> IO Bool
keyPtrNull (Key ptr) = withForeignPtr ptr (return . (== nullPtr))

-- as we use haskell's reference counting here, increase the number by one
-- so it gets deleted properly when haskell calls the finalizer
keyNew :: String -> IO Key
keyNew name = keyNewRaw name KeyEnd >>= addFinalizer
keyNewWithValue :: String -> String -> IO Key
keyNewWithValue name value = keyNewRawWithValue name KeyValue value KeyEnd >>= addFinalizer
keyNewWithFlagsAndValue :: String -> ElektraKeyVarargs -> String -> IO Key
keyNewWithFlagsAndValue name flags value = keyNewRawWithFlagsAndValue name KeyFlags flags KeyValue value KeyEnd >>= addFinalizer
{#fun unsafe variadic keyNew[keyswitch_t] as keyNewRaw {`String', `ElektraKeyVarargs'} -> `Key' #}
{#fun unsafe variadic keyNew[keyswitch_t, const char *, keyswitch_t]
  as keyNewRawWithValue {`String', `ElektraKeyVarargs', `String', `ElektraKeyVarargs'} -> `Key' #}
{#fun unsafe variadic keyNew[keyswitch_t, keyswitch_t, keyswitch_t, const char *, keyswitch_t]
  as keyNewRawWithFlagsAndValue 
    {`String', `ElektraKeyVarargs', `ElektraKeyVarargs', `ElektraKeyVarargs', `String', `ElektraKeyVarargs'} 
    -> `Key' #}
keyDup :: Key -> IO Key
keyDup k = keyDupRaw k >>= addFinalizer
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
keySetBaseName :: Key -> String -> IO Int
keySetBaseName key baseName = keySetBaseNameRaw key (Just baseName)
keyDeleteBaseName :: Key -> IO Int
keyDeleteBaseName key = keySetBaseNameRaw key Nothing
keySetBaseNameRaw :: Key -> Maybe String -> IO Int
keySetBaseNameRaw key baseName = do
  withKey key $ \cKey -> if (isJust baseName)
    then C2HSImp.withCString (fromJust baseName) $ \cValue -> call cKey cValue
    else call cKey nullPtr
  where call cKey cValue = {#call unsafe keySetBaseName as keySetBaseNameRaw' #} cKey cValue
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
{#fun unsafe keyNextMeta as keyNextMeta {`Key'} -> `Key' #}
{#fun unsafe keyCurrentMeta as keyCurrentMeta {`Key'} -> `Key' #}
{#fun unsafe keyCopyMeta {`Key', `Key', `String'} -> `Int' #}
{#fun unsafe keyCopyAllMeta {`Key', `Key'} -> `Int' #}
{#fun unsafe keyGetMeta as keyGetMeta {`Key', `String'} -> `Key' #}
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

addFinalizer :: Key -> IO Key
addFinalizer (Key a) = addForeignPtrFinalizer keyDel a >> return (Key a)

ifKey :: IO a -> (Key -> IO a) -> Key -> IO a
ifKey f t k = keyPtrNull k >>= bool (t k) f 

whenKey :: (Key -> IO ()) -> Key -> IO ()
whenKey w k = ifKey (return ()) w k

-- Temporarily increases the reference counter so e.g. a keyset will not take ownership so
-- Haskell's finalizer can run successfully afterwards
tmpRef :: Key -> IO Key
tmpRef k@(Key a) = keyIncRef k >> addForeignPtrFinalizer keyDecRefPtr a >> return (Key a)

foreign import ccall unsafe "Elektra/Key.chs.h &keyDel"
  keyDel :: C2HSImp.FunPtr ((C2HSImp.Ptr (Key)) -> (IO ()))


foreign import ccall unsafe "Elektra/Key.chs.h &keyDecRef"
  keyDecRefPtr :: C2HSImp.FunPtr ((C2HSImp.Ptr (Key)) -> (IO ()))

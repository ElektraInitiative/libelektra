--
-- @file
--
-- @brief KeySet Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.KeySet (
  KeySet (..), LookupOptions (..), ElektraCursor,
  withKeySet, 
  ksNew, ksDup, ksCopy, ksDel,
  ksGetSize, ksNeedSync,
  ksAppendKey, ksAppend, ksCut, ksPop, 
  ksRewind, ksNext, ksCurrent, ksHead, ksTail, ksList,
  ksGetCursor, ksSetCursor, ksAtCursor,
  ksLookup, ksLookupO, ksLookupByName, ksLookupByNameO
) where

{#import Elektra.Key#}
import Control.Monad (liftM, liftM2, mapM_)
import Data.List (intercalate, deleteFirstsBy)
import System.IO.Unsafe (unsafePerformIO)

#include <elektra/kdb.h>

-- ***
-- TYPE DEFINITIONS
-- ***

{#pointer *KeySet foreign finalizer ksDel newtype #}

-- TODO right now we don't support combinations via |, as most of the options are being
-- phased out its probably not necessary.
{#enum KDB_O_NONE as LookupOptions { underscoreToCase } deriving (Show, Eq) #}

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
ksNew size = ksNewRaw size 0
{#fun unsafe variadic ksNew[int] as ksNewRaw {`Int', `Int'} -> `KeySet' #}
{#fun unsafe ksDup {`KeySet'} -> `KeySet' #}
{#fun unsafe ksCopy {`KeySet', `KeySet'} -> `Int' #}

{#fun unsafe ksGetSize {`KeySet'} -> `Int' #}
{#fun unsafe ksNeedSync {`KeySet'} -> `Int' #}

-- ***
-- KEYSET MODIFICATION
-- ***

-- As Haskell will call keyDel on all Key references, the KeySet is not allowed to take ownership
ksAppendKey :: KeySet -> Key -> IO Int
ksAppendKey ks k = tmpRef k >>= ksAppendKeyRaw ks
{#fun unsafe ksAppendKey as ksAppendKeyRaw {`KeySet', `Key'} -> `Int' #}
ksAppend :: KeySet -> KeySet -> IO Int
ksAppend ks1 ks2 = do
  ksList ks1 >>= (mapM_ tmpRef)
  ksAppendRaw ks1 ks2
{#fun unsafe ksAppend as ksAppendRaw  {`KeySet', `KeySet'} -> `Int' #}
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
ksLookup :: KeySet -> Key -> IO Key
ksLookup ks k = ksLookupO ks k KdbONone
{#fun unsafe ksLookup as ksLookupO {`KeySet', `Key', `LookupOptions'} -> `Key' #}
ksLookupByName :: KeySet -> String -> IO Key
ksLookupByName ks k = ksLookupByNameO ks k KdbONone
{#fun unsafe ksLookupByName as ksLookupByNameO {`KeySet', `String', `LookupOptions'} -> `Key' #}
ksList :: KeySet -> IO [Key]
ksList ks = ksRewind ks >> list []
  where
    list res = do
      cur <- ksNext ks
      isNull <- keyPtrNull cur
      if isNull then return res else liftM (cur :) (list res)

-- ***
-- COMMON HASKELL TYPE CLASSES
-- unsafePerformIO should be ok here, as we use ksDup to "hide" the side effect of ksRewind
-- ***

instance Show KeySet where
    show ks = unsafePerformIO $ ksDup ks >>= ksList >>= return . (intercalate "\n") . (map show)

instance Eq KeySet where
    ks1 == ks2 = null $ unsafePerformIO $ liftM2 (deleteFirstsBy (==)) (ksDup ks1 >>= ksList) (ksDup ks2 >>= ksList)

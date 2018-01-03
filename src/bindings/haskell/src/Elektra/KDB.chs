--
-- @file
--
-- @brief KDB Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.KDB (
  kdbOpen, kdbGet, kdbSet
) where

{#import Elektra.Key#}
{#import Elektra.KeySet#}

#include <kdb.h>

{#pointer *KDB foreign newtype #}

-- ***
-- KDB METHODS
-- ***

kdbOpen :: Key -> (KDB -> IO a) -> IO a
kdbOpen parentKey actions = do
  kdb <- kdbOpenRaw parentKey
  res <- actions kdb
  kdbClose kdb parentKey
  return res
{#fun unsafe kdbOpen as kdbOpenRaw {`Key'} -> `KDB' #}
{#fun unsafe kdbClose {`KDB', `Key'} -> `Int' #}
{#fun unsafe kdbGet {`KDB', `KeySet', `Key'} -> `Int' #}
{#fun unsafe kdbSet {`KDB', `KeySet', `Key'} -> `Int' #}

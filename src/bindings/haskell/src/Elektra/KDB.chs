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

#include <elektra/kdb.h>

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
{#fun kdbOpen as kdbOpenRaw {`Key'} -> `KDB' #}
{#fun kdbClose {`KDB', `Key'} -> `Int' #}
{#fun kdbGet {`KDB', `KeySet', `Key'} -> `Int' #}
{#fun kdbSet {`KDB', `KeySet', `Key'} -> `Int' #}

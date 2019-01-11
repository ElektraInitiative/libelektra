--
-- @file
--
-- @brief LibEase Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Ease (
  ArrayValidateNameResult (..),
  arrayValidateName, arrayIncName, arrayGetNextKey, arrayGet,
  keyGetRelativeName
) where

#include <kdbease.h>

{#import Elektra.Key#}
{#import Elektra.KeySet#}

{#context lib="libelektra" #}

{#fun unsafe elektraKeyGetRelativeName as keyGetRelativeName {`Key', `Key'} -> `String' #}

data ArrayValidateNameResult = Invalid | Start | Element deriving (Show, Eq, Enum)

arrayValidateName :: Key -> IO (ArrayValidateNameResult)
arrayValidateName = fmap parseResult . elektraArrayValidateName
  where
    parseResult 0 = Start
    parseResult 1 = Element
    parseResult _ = Invalid
{#fun unsafe elektraArrayValidateName {`Key'} -> `Int' #}
{#fun unsafe elektraArrayValidateBaseNameString {`String'} -> `Int' #}
{#fun unsafe elektraArrayIncName as arrayIncName {`Key'} -> `Int' #}
{#fun unsafe elektraArrayGet as arrayGet {`Key', `KeySet'} -> `KeySet' #}
{#fun unsafe elektraArrayGetNextKey as arrayGetNextKey {`KeySet'} -> `Key' #}

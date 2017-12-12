module Elektra.Ease (ArrayValidateNameResult (..), keyGetRelativeName, arrayValidateName, arrayIncName, arrayGet, arrayGetNextKey) where

#include <kdbease.h>

{#import Elektra.Key#}
{#import Elektra.KeySet#}

{#context lib="libelektra" #}

{#fun unsafe elektraKeyGetRelativeName as keyGetRelativeName {`Key', `Key'} -> `String' #}

data ArrayValidateNameResult = Invalid | Start | Element deriving (Show, Eq, Enum)

arrayValidateName = fmap parseResult . elektraArrayValidateName
  where
    parseResult 0 = Start
    parseResult 1 = Element
    parseResult _ = Invalid
{#fun unsafe elektraArrayValidateName {`Key'} -> `Int' #}
{#fun unsafe elektraArrayIncName as arrayIncName {`Key'} -> `Int' #}
{#fun unsafe elektraArrayGet as arrayGet {`Key', `KeySet'} -> `KeySet' #}
{#fun unsafe elektraArrayGetNextKey as arrayGetNextKey {`KeySet'} -> `Key' #}

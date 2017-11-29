module Elektra.Ease (keyGetRelativeName) where

#include <kdbease.h>

{#import Elektra.Key#}

{#context lib="libelektra" #}

{#fun unsafe elektraKeyGetRelativeName as keyGetRelativeName {`Key', `Key'} -> `String' #}

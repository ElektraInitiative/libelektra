/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
@from support.elektra_gen import *
@set support = ElektraGenSupport()

#include <stdlib.h>
#include <elektra.h>
#include <kdbhelper.h>
#include "elektra_gen.h"

@for $key, $info in $parameters.iteritems()
@if $support.type_of($info) == "enum"
ELEKTRA_DEFINITIONS ($support.enum_type($key), $support.enum_type_name($key), KDB_TYPE_ENUM, KDB_ENUM_TO_STRING, KDB_STRING_TO_ENUM)
@end if
@end for

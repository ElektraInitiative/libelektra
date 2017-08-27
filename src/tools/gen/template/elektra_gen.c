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

KDBType KDB_TYPE_ENUM = "enum";
#define KDB_ENUM_TO_STRING(value) elektraFormat (ELEKTRA_LONG_F, value)
#define KDB_STRING_TO_ENUM(string) (kdb_long_t) strtoul (string, NULL, 10)

@for $key, $info in $parameters.iteritems()
@if $support.type_of($info) == "enum"
ELEKTRA_DEFINITIONS ($support.enum_type($key), $support.enum_type_name($key), KDB_TYPE_ENUM, KDB_ENUM_TO_STRING, KDB_STRING_TO_ENUM)
@end if
@end for

#undef KDB_ENUM_TO_STRING
#undef KDB_STRING_TO_ENUM
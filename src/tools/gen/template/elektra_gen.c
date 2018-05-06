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
@import os
@set support = ElektraGenSupport()

#include <elektra.h>
#include "$support.header_file($args.output)"

@for $enum in $support.enums($parameters)
ELEKTRA_TAG_DEFINITIONS ($enum.type, $enum.type_name, KDB_TYPE_ENUM, KDB_ENUM_TO_STRING, KDB_STRING_TO_ENUM)
@end for

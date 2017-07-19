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

@for $key, $info in $parameters.iteritems()
@if $support.type_of($info) == "enum"
ELEKTRA_DEFINITIONS ($support.enum_type($key), $support.enum_type_name($key), "enum", KDB_LONG_TO_STRING, KDB_STRING_TO_LONG)
@end if
@end for

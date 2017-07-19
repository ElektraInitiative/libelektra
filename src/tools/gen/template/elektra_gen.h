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

/**
 * Enums
 */

@for $key, $info in $parameters.iteritems()
@if $support.type_of($info) == "enum"
$support.enum_typedef($key, $info)
@end if
@end for

/**
 * Default KeySet
 */

#define ELEKTRA_DEFAULTS ksNew (0, \
@for $key, $info in $parameters.iteritems()
keyNew ("$key", KEY_VALUE, "$support.default_value(key, info)", KEY_META, "type", "$support.type_of(info)", KEY_END), \
@end for
KS_END)

/**
 * Elektra Tags
 */

@for $key, $info in $parameters.iteritems()
#define $support.tag($key) ($support.tag_type(key, info)){"$key"}
@end for

/**
 * Types
 */

@for $key, $info in $parameters.iteritems()
@if $support.type_of($info) == "enum"
ELEKTRA_DECLARATIONS($support.enum_type($key), $support.enum_type_name($key))
@end if
@end for

#undef ELEKTRA_TAG_NAMES_GEN
#define ELEKTRA_TAG_NAMES_GEN(X) \
@for $key, $info in $parameters.iteritems()
@if $support.type_of($info) == "enum"
X($support.enum_type_name($key)) \
@end if
@end for

#include <elektra_generic.h>

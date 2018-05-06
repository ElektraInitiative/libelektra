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

#include <elektra.h>

#ifndef $support.include_guard($args.template)
#define $support.include_guard($args.template)

/**
 * Enum Types
 */

@for $enum in $support.enums($parameters)
$support.enum_typedef($enum)
@end for

/**
 * Enum Tags
 */

@for $enum in $support.enums($parameters)
ELEKTRA_TAG_DECLARATIONS ($enum.type, $enum.type_name)
@end for


/**
 * Default KeySet
 */

#define ELEKTRA_DEFAULTS \
	ksNew ($len($parameters), \
@for $key, $info in $parameters.iteritems()
@if $support.check_default($key, $info)
		keyNew ("$key", KEY_VALUE, "$support.default_value(key, info)", KEY_META, "type", "$support.type_of(info)", KEY_END), \
@end if
@end for
		KS_END)

/**
 * Elektra Tag Values
 */

@for $key, $info in $parameters.iteritems()
ELEKTRA_TAG_VALUE ($support.tag_name($key), "$key", $support.tag_type(key, info))
@end for

#endif // $support.include_guard($args.template)
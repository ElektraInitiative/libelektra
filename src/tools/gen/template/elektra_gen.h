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

#define ELEKTRA_DEFAULTS ksNew (0, \
@for $key, $info in $parameters.iteritems()
keyNew ("$key", KEY_VALUE, "$(info["default"])", KEY_META, "type", "$support.type_of(info)", KEY_END), \
@end for
KS_END)

@for $key, $info in $parameters.iteritems()
#define $support.tag_name($key) ($support.tag_type_of(info)){"$key"}
@end for

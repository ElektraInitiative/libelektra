/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
@from util import util
@from support.c import *
@set support = CSupport()
$util.header($args.output)
#include "elektra/kdb.h"
#include "elektra/kdbtypes.h"

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

// for strol
#include <limits.h>
#include <errno.h>


@for $k, $i in $parameters.iteritems():
@if support.isenum(i):
/**
 * Enum of $k
 */
$support.typeof(i)
{
@for e in $support.enumval(i)
	$e,
@end for
};

/** @brief Convert enum to string
 *
 * \return string that holds value of enum
 * \param e the enum that should be converted
 */
static inline const char *${support.enumname(i)}_to_string($support.typeof(i) e)
{
	switch (e)
	{
@for e in $support.enumval(i)
	case $e: return "$e";
@end for
	}
	return "";
}

/** @brief Convert enum from string
 *
 * \return enum from string s or default value
 * \param s the string that should be converted
 */
static inline $support.typeof(i) ${support.enumname(i)}_from_string(const char *s)
{
	$support.typeof(i) ret $support.valof(i)
@for e in $support.enumval(i)
	if (!strcmp(s, "$e"))
		ret = $e;
@end for
	return ret;
}

@end if
@end for

/**
 * @brief Convert bool to string
 * \param b bool to convert (0 is false)
 */
static inline const char *bool_to_string(int b)
{
	if (b==0)
	{
		return "false";
	}
	return "true";
}

/**
 * @brief Convert string to bool
 * \param s string to convert (true, 1 or on is true)
 */
static inline int bool_from_string(const char *s)
{
	if (
	   !strcmp(s, "${support.trueval()[0]}")
@for b in $support.trueval()[1:]
	   || !strcmp(s, "$b")
@end for
	   )
		return 1;
	else
		return 0;
}

@for $key, $info in $parameters.iteritems()
/** @brief Get parameter $key
 *
 * $util.doxygen(support, key, info)
 *
 * \see $support.setfuncname($key)
 *
 * \return the value of the parameter, default if it could not be found
 * \param ks the keyset where the parameter is searched
 */
static inline $support.typeof(info) $support.getfuncname($key)(KeySet *ks)
{
@if len(support.override(info)) > 0
	// override
	Key * searchKey = keyNew("${support.override(info)[0]}",
		KEY_END);
	Key * found = ksLookup(ks, searchKey, 0);
@for $o in $support.override(info)[1:]
	if (!found)
	{
		keySetName(searchKey, "$o");
		found = ksLookup(ks, searchKey, 0);
	}
@end for
	// now the key itself
	if (!found)
	{

		keySetName(searchKey, "$key");
		found = ksLookup(ks, searchKey, 0);
	}
@else
	Key * searchKey = keyNew("${key}",
		KEY_END);
	Key * found = ksLookup(ks, searchKey, 0);
@end if

@if len($support.fallback(info)) > 0
	// fallback
@for $f in $support.fallback(info)
	if (!found)
	{
		keySetName(searchKey,  "$f");
		found = ksLookup(ks, searchKey, 0);
	}
@end for
@end if
	keyDel(searchKey);

@def strtonumber(support, info, function)
char *endptr;
		errno = 0;
@if function == 'strtof' or function == 'strtod' or function == 'strtold'
		ret = ${function}(keyString(found), &endptr);
@else
		ret = ${function}(keyString(found), &endptr, 10);
@end if
		if ((errno == ERANGE
				&& (ret == LONG_MAX || ret == LONG_MIN))
				|| (errno != 0 && ret == 0))
		{
			ret ${support.valof(info)}
		}

		if (endptr == keyString(found))
		{
		
			ret ${support.valof(info)}
		}
@end def
	$support.typeof(info) ret $support.valof(info)

	if (found)
	{
	@if $info['type'] == 'short'
		$strtonumber(support, info, "strtol")
	@else if $info['type'] == 'long'
		$strtonumber(support, info, "strtoul")
	@else if $info['type'] == 'long_long'
		$strtonumber(support, info, "ELEKTRA_LONG_LONG_S")
	@else if $info['type'] == 'unsigned_short'
		$strtonumber(support, info, "strtoul")
	@else if $info['type'] == 'unsigned_long'
		$strtonumber(support, info, "strtoul")
	@else if $info['type'] == 'unsigned_long_long'
		$strtonumber(support, info, "ELEKTRA_UNSIGNED_LONG_LONG_S")
	@else if $info['type'] == 'float'
		$strtonumber(support, info, "strtof")
	@else if $info['type'] == 'double'
		$strtonumber(support, info, "strtod")
	@else if $info['type'] == 'long_double'
		$strtonumber(support, info, "strtold")
	@else if $info['type'] == 'char'
		ret = keyString(found)[0];
	@else if $info['type'] == 'octet'
		$strtonumber(support, info, "strtol")
	@else if $info['type'] == 'string'
		ret = keyString(found);
	@else if $info['type'] == 'boolean'
		ret = bool_from_string(keyString(found));
	@else if $support.isenum(info)
		ret = ${support.enumname(info)}_from_string(keyString(found));
	@end if
	}

	return ret;
}

/** @brief Set parameter $key
 *
 * $util.doxygen(support, key, info)
 *
 * \see $support.setfuncname($key)
 *
 * \param ks the keyset where the parameter is added or replaced
 * \param n is the value to set in the parameter
 */
static inline void $support.setfuncname($key)(KeySet *ks, $support.typeof(info) n)
{
	Key * found = ksLookupByName(ks, "$key", 0);
@if $info['type'] == 'short'
	char s[100];
	snprintf(s, 99, "%hd", n);
@else if $info['type'] == 'long'
	char s[100];
	snprintf(s, 99, ELEKTRA_LONG_F, n);
@else if $info['type'] == 'long_long'
	char s[100];
	snprintf(s, 99, ELEKTRA_LONG_LONG_F, n);
@else if $info['type'] == 'unsigned_short'
	char s[100];
	snprintf(s, 99, "%hu", n);
@else if $info['type'] == 'unsigned_long'
	char s[100];
	snprintf(s, 99, ELEKTRA_UNSIGNED_LONG_F, n);
@else if $info['type'] == 'unsigned_long_long'
	char s[100];
	snprintf(s, 99, ELEKTRA_UNSIGNED_LONG_LONG_F, n);
@else if $info['type'] == 'float'
	char s[100];
	snprintf(s, 99, "%f", n);
@else if $info['type'] == 'double'
	char s[100];
	snprintf(s, 99, "%f", n);
@else if $info['type'] == 'long_double'
	char s[100];
	snprintf(s, 99, "%Lf", n);
@else if $info['type'] == 'char'
	char s[100];
	snprintf(s, 99, "%c", n);
@else if $info['type'] == 'octet'
	char s[100];
	snprintf(s, 99, "%hd", n);
@else if $info['type'] == 'string'
	const char *s = n;
@else if $info['type'] == 'boolean'
	const char *s = bool_to_string(n);
@else if $support.isenum(info)
	const char *s = ${support.enumname(info)}_to_string(n);
@end if
	if (!found)
	{
		ksAppendKey(ks, keyNew("$support.userkey(key)",
				KEY_VALUE, s,
				KEY_END));
	}
	else
	{
		keySetString(found, s);
	}
}


@end for
$util.footer($args.output)

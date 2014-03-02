#from c_support import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
#ifndef $includeguard($args.output)
#define $includeguard($args.output)
/** \file
 *
 * Generated file ${args.output}
 * With include guard $includeguard($args.output)
 *
 * \warning this is a generated file, do not modify it
 * \warning this is a prototype and not production code
 */
#include "kdb.h"
#include "kdbtypes.h"

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

// for strol
#include <limits.h>
#include <errno.h>


@for $key, $info in $parameters.items()
@if $isenum(info):
/**
 * Enum of $key
 */
$typeof(info)
{
@for $enum in $enumval(info)
	$enum,
@end for
};

/** \brief Convert enum to string
 *
 * \return string that holds value of enum
 * \param e the enum that should be converted
 */
static inline const char *${enumname(info)}_to_string($typeof(info) e)
{
	switch(e)
	{
@for $enum in $enumval(info)
	case $enum: return "$enum";
@end for
	}
	return "";
}

/** \brief Convert enum from string
 *
 * \return enum from string s or default value
 * \param s the string that should be converted
 */
static inline $typeof(info) ${enumname(info)}_from_string(const char *s)
{
	$typeof(info) ret $valof(info)
@for $enum in $enumval(info)
	if(!strcmp(s, "$enum"))
		ret = $enum;
@end for
	return ret;
}

@end if
@end for

/**
 * \brief Convert bool to string
 * \param b bool to convert (0 is false)
 */
static inline const char *bool_to_string(int b)
{
	if(b==0)
	{
		return "false";
	}
	return "true";
}

/**
 * \brief Convert string to bool
 * \param s string to convert (true, 1 or on is true)
 */
static inline int bool_from_string(const char *s)
{
	if(
	   !strcmp(s, "${trueval()[0]}")
@for $b in $trueval()[1:]
	   || !strcmp(s, "$b")
@end for
	   )
		return 1;
	else
		return 0;
}

##todo: duplicate
@def doxygen(key, info)
 * \par Type
 * $info['type']
 * \par Mapped Type
 * $typeof(info)
@if $info.get('unit'):
 * \par Unit
 * $info.get('unit')
@end if
 * \par Default Value
 * $info['default']
@if $info.get('explanation'):
 * \par Explanation
 * $info.get('explanation')
@end if
@if $info.get('rationale'):
 * \par Rationale
 * $info.get('rationale')
@end if
@if $info.get('override')
 * \par Override
<ul>
    @for $i in $override(info)
    <li>get_${funcname($i)}()</li>
    @end for
</ul>
@end if
@if $info.get('fallback')
 * \par Fallback
<ul>
    @for $i in $fallback(info)
    <li>get_${funcname($i)}()</li>
    @end for
</ul>
@end if
@if $info.get('see')
    @for $i in $see(info)
 * \see get_${funcname($i)}
    @end for
@end if
@end def

@for $key, $info in $parameters.items()
/** \brief Get parameter $key
 *
 * $doxygen(key, info)
 *
 * \see set_$funcname($key)
 *
 * \return the value of the parameter, default if it could not be found
 * \param ks the keyset where the parameter is searched
 */
static inline $typeof(info) get_$funcname($key)(KeySet *ks)
{
@if $info.get('override')
	// override
	Key * found = ksLookupByName(ks, "${override(info)[0]}", 0);
@for $o in $override(info)[1:]
	if (!found)
	{
		found = ksLookupByName(ks, "$o", 0);
	}
@end for
	// now the key itself
	if(!found)
	{
		found = ksLookupByName(ks, "$key", 0);
	}
@else
	Key * found = ksLookupByName(ks, "$key", 0);
@end if

@if $info.get('fallback')
	// fallback
@for $f in $fallback(info)
	if (!found)
	{
		found = ksLookupByName(ks, "$f", 0);
	}
@end for
@end if

@def strtonumber(info, function)
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
			ret $valof(info)
		}

		if (endptr == keyString(found))
		{
		
			ret $valof(info)
		}
@end def
	$typeof(info) ret $valof(info)

	if(found)
	{
	@if $info['type'] == 'short'
		$strtonumber(info, "strtol")
	@else if $info['type'] == 'long'
		$strtonumber(info, "strtoul")
	@else if $info['type'] == 'long_long'
		$strtonumber(info, "ELEKTRA_LONG_LONG_S")
	@else if $info['type'] == 'unsigned_short'
		$strtonumber(info, "strtoul")
	@else if $info['type'] == 'unsigned_long'
		$strtonumber(info, "strtoul")
	@else if $info['type'] == 'unsigned_long_long'
		$strtonumber(info, "ELEKTRA_UNSIGNED_LONG_LONG_S")
	@else if $info['type'] == 'float'
		$strtonumber(info, "strtof")
	@else if $info['type'] == 'double'
		$strtonumber(info, "strtod")
	@else if $info['type'] == 'long_double'
		$strtonumber(info, "strtold")
	@else if $info['type'] == 'char'
		ret = keyString(found)[0];
	@else if $info['type'] == 'octet'
		$strtonumber(info, "strtol")
	@else if $info['type'] == 'string'
		ret = keyString(found);
	@else if $info['type'] == 'boolean'
		ret = bool_from_string(keyString(found));
	@else if $isenum(info)
		ret = ${enumname(info)}_from_string(keyString(found));
	@end if
	}

	return ret;
}

/** \brief Set parameter $key
 *
 * $doxygen(key, info)
 *
 * \see set_$funcname($key)
 *
 * \param ks the keyset where the parameter is added or replaced
 * \param n is the value to set in the parameter
 */
static inline void set_$funcname($key)(KeySet *ks, $typeof(info) n)
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
@else if $isenum(info)
	const char *s = ${enumname(info)}_to_string(n);
@end if
	if(!found)
	{
		ksAppendKey(ks, keyNew("$userkey(key)",
				KEY_VALUE, s,
				KEY_END));
	}
	else
	{
		keySetString(found, s);
	}
}


@end for
#endif // $includeguard($args.output)

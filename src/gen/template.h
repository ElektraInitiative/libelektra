#from c_support import *
#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
#ifndef ELEKTRA_GEN_FILENAME_H
#define ELEKTRA_GEN_FILENAME_H
/** \file
 * \warning this is a generated file, do not modify it
 * \warning this is a prototype and not production code
 */
#include "kdb.h"
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

/** Parse commandline options and append it to keyset
 * \param argc the argument counter
 * \param argv the argument string array
 * \param ks the keyset to store the configuration to
 * needs template_getopt.c
 */
int ksGetOpt(int argc, char **argv, KeySet *ks);

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
	if(!strcmp(s, "true") ||
	   !strcmp(s, "1") ||
	   !strcmp(s, "on"))
		return 1;
	else
		return 0;
}

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
	Key * found = ksLookupByName(ks, "$key", 0);
	$typeof(info) ret $valof(info)

	if(found)
	{
	@if $info['type'] == 'unsigned_int_32'
		ret = atoi(keyString(found));
	@else if $info['type'] == 'double'
		ret = atof(keyString(found));
	@else if $info['type'] == 'string'
		ret = keyString(found);
	@else if $info['type'] == 'bool'
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
@if $info['type'] == 'unsigned_int_32'
	char s[100];
	snprintf(s, 99, "%d", n);
@else if $info['type'] == 'double'
	char s[100];
	snprintf(s, 99, "%f", n);
@else if $info['type'] == 'string'
	const char *s = n;
@else if $info['type'] == 'bool'
	const char *s = bool_to_string(n);
@else if $isenum(info)
	const char *s = ${enumname(info)}_to_string(n);
@end if
	if(!found)
	{
		ksAppendKey(ks, keyNew("$key",
				KEY_VALUE, s,
				KEY_END));
	}
	else
	{
		keySetString(found, s);
	}
}


@end for
#endif

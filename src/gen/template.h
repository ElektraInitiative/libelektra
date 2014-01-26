#from gen_support import *
#from c_support import *

#compiler-settings
directiveStartToken = @
cheetahVarStartToken = $
#end compiler-settings
// start of a generated file
#include "kdb.h"
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

@for $key, $info in $parameters.items()
@if $isenum(info):
$typeof(info)
{
@for $enum in $enumval(info)
	$enum,
@end for
};

/**
  * \@return string that holds value of enum
  * \@param e the enum that should be converted
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

/**
  * \@return enum from string s or default value
  * \@param s the string that should be converted
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

static inline const char *bool_to_string(int b)
{
	if(b==0)
	{
		return "false";
	}
	return "true";
}

static inline int bool_from_string(const char *s)
{
	if(!strcmp(s, "true") ||
	   !strcmp(s, "1") ||
	   !strcmp(s, "on"))
		return 1;
	else
		return 0;
}

@for $key, $info in $parameters.items()
/**
 * Type: $info['type']
 * Mapped Type: $typeof(info)
 * Default Value: $info['default']
 * Description: $info.get('explanation')
 *
 * \@warning this is a prototype and not production code
 *
 * \@return the value of the parameter, default if it could not be found
 * \@param ks the keyset where the parameter is searched
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

/**
 *
 * \@warning this is a prototype and not production code
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
// end of a generated file

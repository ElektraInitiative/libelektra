/**
 * @file
 *
 * @brief memoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#define _XOPEN_SOURCE
#include "memoryvalue.h"
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbtypes.h>
#include <regex.h>
#include <stdlib.h>
#include <stdio.h>

typedef enum
{
	MEMORY_INVALID,
	MEMORY_BYTE,
	MEMORY_KILOBYTE,
	MEMORY_MEGABYTE,
	MEMORY_GIGABYTE,
	MEMORY_TERABYTE,
	MEMORY_PETABYTE,
} MemoryFormat;

static MemoryFormat is_valid_key(Key * key, Key * parentKey){

	const Key * meta = keyGetMeta (key, "check/memoryvalue");
	const char * pattern = "\d* ?[B,KB,MB,GB,TB,PB]";
	const char * value = keyString (key);
	regmatch_t offsets;
	regex_t regex;
	int compile_failure;
	int match;

	if (!meta) {
		return 1;
	}

	compile_failure = regcomp (&regex, pattern, REG_NOSUB | REG_EXTENDED | REG_NEWLINE);

	if (compile_failure){
		 return 1;
	}

	match = !(regexec (&regex, value, 0, &offsets, 0));
	regfree (&regex);

	if (!match)
	{
		return MEMORY_INVALID;
	}
	if(strstr(value, "B") != NULL) 
	{
		return MEMORY_BYTE;
	}
	if(strstr(value, "KB") != NULL)
	{
		return MEMORY_KILOBYTE;
	}
	if(strstr(value, "MB") != NULL)
	{
		return MEMORY_MEGABYTE;
	}
	if(strstr(value, "GB") != NULL)
	{
		return MEMORY_GIGABYTE;
	}
	if(strstr(value, "TB") != NULL)
	{
		return MEMORY_TERABYTE;
	}
	if(strstr(value, "PB") != NULL) 
	{
		return MEMORY_PETABYTE;
	}else{
		return MEMORY_INVALID;
	}
}


static void elektraMemoryvalueConvertToByteString (Key * key, MemoryFormat format){

	const char * str = keyString (key);
	keySetMeta (key, "unprocessedvalue", str);
    char *ptr;
	long ret;
	kdb_unsigned_long_t normalizedMemVal;

	ret = strtol(str, &ptr, 10);
	normalizedMemVal=ret;

	//normalize to byte string
	switch(format){
		case MEMORY_KILOBYTE:
		normalizedMemVal=ret*1000;
		break;
		case MEMORY_MEGABYTE:
		normalizedMemVal=ret*1000*1000;
		break;
		case MEMORY_GIGABYTE:
		normalizedMemVal=ret*1000*1000;
		break;
		case MEMORY_TERABYTE:
		normalizedMemVal=ret*1000*1000*1000;
		break;
		case MEMORY_PETABYTE:
		normalizedMemVal=ret*1000*1000*1000*1000;
		break;
		default:
		normalizedMemVal=ret;
		break;
	}
	//convert back to string
	const int n = snprintf(NULL, 0, "%lu", normalizedMemVal);
	char buf[n+1];
	int c = snprintf(buf, n+1, "%lu", normalizedMemVal);

	keySetString (key, buf);
}

static void elektraMemoryvalueRestore (Key * key)
{
	const Key * oldval = keyGetMeta (key, "unprocessedvalue");
	if (oldval != NULL)
	{
		keySetString (key, keyString (oldval));
	}
}


int elektraMemoryvalueGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/memoryvalue"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/memoryvalue", KEY_VALUE, "memoryvalue plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/memoryvalue/exports", KEY_END),
			       keyNew ("system/elektra/modules/memoryvalue/exports/get", KEY_FUNC, elektraMemoryvalueGet, KEY_END),
			       keyNew ("system/elektra/modules/memoryvalue/exports/set", KEY_FUNC, elektraMemoryvalueSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/memoryvalue/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * cur;
	int rc = 1;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/memoryvalue");
		if (meta)
		{
			MemoryFormat format = is_valid_key (cur, parentKey);

			if (format == MEMORY_INVALID){ 
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			elektraMemoryvalueConvertToByteString(cur, format);
		}
	}
	return rc; 
}

int elektraMemoryvalueSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/memoryvalue");
		if (!meta){
 			continue;
		}

		elektraMemoryvalueRestore (cur);
		MemoryFormat format = is_valid_key (cur, parentKey);

		if (format == MEMORY_INVALID){ 
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		elektraMemoryvalueConvertToByteString (cur, format);
		
	}
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

}


Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("memoryvalue",
		ELEKTRA_PLUGIN_GET,	&elektraMemoryvalueGet,
		ELEKTRA_PLUGIN_SET,	&elektraMemoryvalueSet,
		ELEKTRA_PLUGIN_END);
}

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <fnmatch.h>
#include <kdbhelper.h>
#include <kdbease.h>

#include "metafunctions.h"


/*
 * creates an metadata array or appends another element to an existing metadata array
 * e.g.
 * Key *key = keyNew("user/test", KEY_END);
 * elektraMetaArrayAdd(key, "array", "test0");
 * key now has "test/#0" with value "test0" as metadata
 * elektraMetaArrayAdd(key, "array", "test1");
 * appends "test/#1" with value "test1" to key
 */

void elektraMetaArrayAdd (Key * key, const char * metaName, const char * value)
{
	const Key *meta = keyGetMeta(key, metaName);
	Key *arrayKey;
	if(!meta)
	{
		keySetMeta(key, metaName, "#0");
		arrayKey = keyDup(keyGetMeta(key, metaName));
		keySetString(arrayKey, 0);
		keyAddBaseName(arrayKey, "#");
	}
	else
	{
		arrayKey = keyDup(meta);
		keyAddBaseName(arrayKey, keyString(meta));
	}
	elektraArrayIncName(arrayKey);
	keySetMeta(key, keyName(arrayKey), value);
	keySetMeta(key, metaName, keyBaseName(arrayKey));
	keyDel(arrayKey);
}

/*
 * returns the metakey array as a string separated by delim
 * the return value must be freed
 */

char * elektraMetaArrayToString (Key * key, const char * metaName, const char * delim)
{
		char *result = NULL;
		Key *lookupElem = keyDup(keyGetMeta(key, metaName));
		keyAddBaseName(lookupElem, "#0");
		Key *elem = (Key *)keyGetMeta(key, keyName(lookupElem));
		if(elem != NULL)
		{
			elektraRealloc((void **)&result, keyGetValueSize(elem));
			snprintf(result, keyGetValueSize(elem), "%s", keyString(elem));
		}
		elektraArrayIncName(lookupElem);
		elem = (Key *)keyGetMeta(key, keyName(lookupElem));
		while(elem != NULL)
		{
			elektraRealloc((void **)&result, elektraStrLen(result)+keyGetValueSize(elem)+1); //String (incl. +2 times \0) + delimiter + whitespace
			strcat(result, delim);
			strcat(result, keyString(elem));
			elektraArrayIncName(lookupElem);
			elem = (Key *)keyGetMeta(key, keyName(lookupElem));
		}
		keyDel(lookupElem);
		return result;
}

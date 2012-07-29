/***************************************************************************
                     yajl.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "yajl.h"

#include <kdberrors.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <yajl/yajl_gen.h>
#include <yajl/yajl_parse.h>

#undef ELEKTRA_YAJL_VERBOSE

/**
 @retval 0 if ksCurrent does not hold an array entry
 @retval 1 if the array entry will be used because its the first
 @retval 2 if a new array entry was created
 @retval -1 error in snprintf
 */
static int increment_array_entry(KeySet * ks)
{
	Key * current = ksCurrent(ks);

	if (keyGetMeta(current, "array"))
	{
		const char * baseName = keyBaseName(current);

		if (!strcmp(baseName, "###start_array"))
		{
			// we have a new array entry, just use it
			keySetBaseName (current, "0");
			return 1;
		}
		else
		{
			// we are in an array
			const int maxDigitsOfNumber = 10;
			int nextNumber = atoi (baseName) + 1;
			Key * newKey = keyNew (keyName(current), KEY_END);
			char str[maxDigitsOfNumber+1];
			if (snprintf (str, maxDigitsOfNumber, "%d", nextNumber) < 0)
			{
				return -1;
			}
			keySetBaseName(newKey, str);
			keySetMeta(newKey, "array", "");
			ksAppendKey(ks, newKey);
			return 2;
		}
	}
	else
	{
		// previous entry indicates this is not an array
		return 0;
	}
}

static int parse_null(void *ctx)
{
	KeySet *ks = (KeySet*) ctx;
	increment_array_entry(ks);

	Key * current = ksCurrent(ks);

	keySetBinary(current, NULL, 0);

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_null\n");
#endif

	return 1;
}

static int parse_boolean(void *ctx, int boolean)
{
	KeySet *ks = (KeySet*) ctx;
	increment_array_entry(ks);

	Key * current = ksCurrent(ks);

	if (boolean == 1)
	{
		keySetString(current, "true");
	}
	else
	{
		keySetString(current, "false");
	}
	keySetMeta(current, "type", "boolean");

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_boolean %d\n", boolean);
#endif

	return 1;
}

static int parse_number(void *ctx, const char *stringVal,
			unsigned int stringLen)
{
	KeySet *ks = (KeySet*) ctx;
	increment_array_entry(ks);

	Key *current = ksCurrent(ks);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char*)stringVal;
	stringValue[stringLen] = '\0';

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_number %s %d\n", stringVal, stringLen);
#endif

	keySetString(current, stringVal);
	keySetMeta(current, "type", "number");

	// restore old character in buffer
	stringValue[stringLen] = delim;

	return 1;
}

static int parse_string(void *ctx, const unsigned char *stringVal,
			unsigned int stringLen)
{
	KeySet *ks = (KeySet*) ctx;
	increment_array_entry(ks);

	Key *current = ksCurrent(ks);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char*)stringVal;
	stringValue[stringLen] = '\0';

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_string %s %d\n", stringVal, stringLen);
#endif

	keySetString(current, stringValue);

	// restore old character in buffer
	stringValue[stringLen] = delim;
	return 1;
}

static int parse_map_key(void *ctx, const unsigned char * stringVal,
			 unsigned int stringLen)
{
	KeySet *ks = (KeySet*) ctx;
	increment_array_entry(ks);

	Key *currentKey = ksCurrent(ks);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char*)stringVal;
	stringValue[stringLen] = '\0';

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_map_key stringValue: %s currentKey: %s\n", stringValue,
			keyName(currentKey));
#endif
	if (!strcmp(keyBaseName(currentKey), "###start_map"))
	{
		// now we know the name of the object
		keySetBaseName(currentKey, stringValue);
	}
	else
	{
		// we entered a new pair (inside the previous object)
		Key * newKey = keyNew (keyName(currentKey), KEY_END);
		keySetBaseName(newKey, stringValue);
		ksAppendKey(ks, newKey);
	}

	// restore old character in buffer
	stringValue[stringLen] = delim;

	return 1;
}

static int parse_start_map(void *ctx)
{
	KeySet *ks = (KeySet*) ctx;
	increment_array_entry(ks);

	Key *currentKey = ksCurrent(ks);

	Key * newKey = keyNew (keyName(currentKey), KEY_END);
	keyAddBaseName(newKey, "###start_map");
	ksAppendKey(ks, newKey);

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_start_map with new key %s\n", keyName(newKey));
#endif

	return 1;
}

static int parse_end(void *ctx)
{
	KeySet *ks = (KeySet*) ctx;
	Key *currentKey = ksCurrent(ks);

	Key * lookupKey = keyNew (keyName(currentKey), KEY_END);
	keySetBaseName(lookupKey, ""); // remove current key

	// lets point to the correct place
	Key * foundKey = ksLookup(ks, lookupKey, 0);
	(void)foundKey;

#ifdef ELEKTRA_YAJL_VERBOSE
	if (foundKey)
	{
		printf ("parse_end %s\n", keyName(foundKey));
	}
	else
	{
		printf ("parse_end did not find key!\n");
	}
#endif

	keyDel (lookupKey);

	return 1;
}

static int parse_end_map(void *ctx)
{
	return parse_end(ctx);
}

static int parse_start_array(void *ctx)
{
	KeySet *ks = (KeySet*) ctx;
	increment_array_entry(ks);

	Key *currentKey = ksCurrent(ks);

	Key * newKey = keyNew (keyName(currentKey), KEY_END);
	keyAddBaseName(newKey, "###start_array");
	keySetMeta(newKey, "array", "");
	ksAppendKey(ks, newKey);

#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_start_array with new key %s\n", keyName(newKey));
#endif

	return 1;
}

static int parse_end_array(void *ctx)
{
	return parse_end(ctx);
}

int elektraYajlGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	yajl_callbacks callbacks = {
		parse_null,
		parse_boolean,
		NULL,
		NULL,
		parse_number,
		parse_string,
		parse_start_map,
		parse_map_key,
		parse_end_map,
		parse_start_array,
		parse_end_array
	};

	if (!strcmp (keyName(parentKey), "system/elektra/modules/yajl"))
	{
		KeySet *moduleConfig = ksNew (30,
			keyNew ("system/elektra/modules/yajl",
				KEY_VALUE, "yajl plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/yajl/exports", KEY_END),
			keyNew ("system/elektra/modules/yajl/exports/get",
				KEY_FUNC, elektraYajlGet,
				KEY_END),
			keyNew ("system/elektra/modules/yajl/exports/set",
				KEY_FUNC, elektraYajlSet,
				KEY_END),
			keyNew ("system/elektra/modules/yajl/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/yajl/infos/author",
				KEY_VALUE, "Markus Raab <elektra@libelektra.org>", KEY_END),
			keyNew ("system/elektra/modules/yajl/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/yajl/infos/description",
				KEY_VALUE, "JSON using YAIL", KEY_END),
			keyNew ("system/elektra/modules/yajl/infos/provides",
				KEY_VALUE, "storage", KEY_END),
			keyNew ("system/elektra/modules/yajl/infos/placements",
				KEY_VALUE, "getstorage setstorage", KEY_END),
			keyNew ("system/elektra/modules/yajl/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/yajl/infos/recommends",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/yajl/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			keyNew ("system/elektra/modules/yajl/config", KEY_END),
			keyNew ("system/elektra/modules/yajl/config/system_path",
				KEY_VALUE, "system",
				KEY_END),
			keyNew ("system/elektra/modules/yajl/config/user_path",
				KEY_VALUE, "user",
				KEY_END),
			KS_END);
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	KeySet *config= elektraPluginGetConfig(handle);

	ksClear (returned);
	if (keyIsUser(parentKey))
	{
		const Key * lookup = ksLookupByName(config, "/user_path", 0);
		if (!lookup)
		{
			ksAppendKey (returned, keyNew("user", KS_END));
		} else {
			ksAppendKey (returned, keyNew(keyValue(lookup),
						KS_END));
		}
	}
	else
	{
		const Key * lookup = ksLookupByName(config, "/system_path", 0);
		if (!lookup)
		{
			ksAppendKey (returned, keyNew("system", KS_END));
		} else {
			ksAppendKey (returned, keyNew(keyValue(lookup),
						KS_END));
		}
	}

	// allow comments
	yajl_parser_config cfg = { 1, 1 };
	yajl_handle hand = yajl_alloc(&callbacks, &cfg, NULL, returned);

	unsigned char fileData[65536];
	int done = 0;
	FILE * fileHandle = fopen(keyString(parentKey), "r");
	if (!fileHandle)
	{
		ELEKTRA_SET_ERROR(75, parentKey, keyString(parentKey));
		return -1;
	}

	while (!done)
	{
		size_t rd = fread(	(void *) fileData, 1,
					sizeof(fileData) - 1,
					fileHandle);
		if (rd == 0)
		{
			if (!feof(fileHandle))
			{
				ELEKTRA_SET_ERROR(76, parentKey, keyString(parentKey));
				fclose (fileHandle);
				return -1;
			}
			done = 1;
		}
		fileData[rd] = 0;

		yajl_status stat;
		if (done)
		{
			stat = yajl_parse_complete(hand);
		}
		else
		{
			stat = yajl_parse(hand, fileData, rd);
		}

		if (stat != yajl_status_ok &&
		    stat != yajl_status_insufficient_data)
		{
			unsigned char * str = yajl_get_error(hand, 1,
					fileData, rd);
			ELEKTRA_SET_ERROR(77, parentKey, (char*)str);
			yajl_free_error(hand, str);
			fclose (fileHandle);

			return -1;
		}
	}

	yajl_free(hand);
	fclose (fileHandle);

	return 1; /* success */
}

int elektraYajlSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	yajl_gen_config conf = { 1, "  " };
	yajl_gen g = yajl_gen_alloc(&conf, NULL);
	yajl_gen_map_open(g);

	Key *cur = 0;

	ksRewind (returned);
	while ((cur = ksNext(returned)) != 0)
	{
		yajl_gen_string(g, (const unsigned char *)keyName(cur), keyGetNameSize(cur)-1);
		if (keyGetValueSize(cur))
		{
			yajl_gen_string(g, (const unsigned char *)keyString(cur), keyGetValueSize(cur)-1);
		} else {
			yajl_gen_null(g);
		}
	}

	yajl_gen_map_close(g);


	FILE *fp = fopen(keyString(parentKey), "w");
	if (!fp)
	{
		ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		return -1;
	}

	const unsigned char * buf;
	unsigned int len;
	yajl_gen_get_buf(g, &buf, &len);
	fwrite(buf, 1, len, fp);
	yajl_gen_clear(g);
	yajl_gen_free(g);


	fclose (fp);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(yajl)
{
	return elektraPluginExport("yajl",
		ELEKTRA_PLUGIN_GET,	&elektraYajlGet,
		ELEKTRA_PLUGIN_SET,	&elektraYajlSet,
		ELEKTRA_PLUGIN_END);
}


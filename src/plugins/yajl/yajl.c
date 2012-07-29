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

#define ELEKTRA_YAJL_VERBOSE 1


int elektraYajlOpen(Plugin *handle, Key *errorKey)
{
	/* plugin initialization logic */

	return 1; /* success */
}

int elektraYajlClose(Plugin *handle, Key *errorKey)
{
	/* free all plugin resources and shut it down */

	return 1; /* success */
}

static int parse_null(void *ctx)
{
	KeySet *ks = (KeySet*) ctx;
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
	Key * current = ksCurrent(ks);
	if (boolean == 1)
	{
		printf ("setString of %s to true\n", keyName(current));
		keySetString(current, "true");
	}
	else
	{
		printf ("setString of %s to false\n", keyName(current));
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
#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_number %s %d\n", stringVal, stringLen);
#endif

	return 1;
}

static int parse_string(void *ctx, const unsigned char *stringVal,
			unsigned int stringLen)
{
	return 1;
}

static int parse_map_key(void *ctx, const unsigned char * stringVal,
			 unsigned int stringLen)
{
	KeySet *ks = (KeySet*) ctx;
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
		Key * newKey = keyDup (currentKey);
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
	Key *currentKey = ksCurrent(ks);

	Key * newKey = keyDup (currentKey);
	keyAddBaseName(newKey, "###start_map");
	ksAppendKey(ks, newKey);
#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_start_map with new key %s\n", keyName(newKey));
#endif

	return 1;
}

static int parse_end_map(void *ctx)
{
#ifdef ELEKTRA_YAJL_VERBOSE
	printf ("parse_end_map\n");
#endif
	return 1;
}

static int parse_start_array(void *ctx)
{
	return 1;
}

static int parse_end_array(void *ctx)
{
	return 1;
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

	ksClear (returned);
	if (keyIsUser(parentKey))
	{
		ksAppendKey (returned, keyNew("user", KS_END));
	}
	else
	{
		ksAppendKey (returned, keyNew("system", KS_END));
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
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(yajl)
{
	return elektraPluginExport("yajl",
		ELEKTRA_PLUGIN_OPEN,	&elektraYajlOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraYajlClose,
		ELEKTRA_PLUGIN_GET,	&elektraYajlGet,
		ELEKTRA_PLUGIN_SET,	&elektraYajlSet,
		ELEKTRA_PLUGIN_END);
}


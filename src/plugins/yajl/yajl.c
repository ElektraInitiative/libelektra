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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>
#include <kdberrors.h>

#include <yajl/yajl_gen.h>
#include <yajl/yajl_parse.h>
#include <yajl/yajl_version.h>

#if YAJL_MAJOR == 1
	typedef unsigned int yajl_size_type;
#else
	typedef size_t yajl_size_type;
#endif

#undef ELEKTRA_YAJL_VERBOSE

/**
 * @brief Only works from 0..9
 *
 * @param key which base name will be incremented
 *
 * @retval -1 on error
 * @retval 0 on success
 */
int keyArrayIncName(Key *key)
{
	if (!key)
	{
		return -1;
	}

	const char * baseName = keyBaseName(key);
	if (!baseName)
	{
		return -1;
	}
	else if (*baseName != '#')
	{
		return -1;
	}

	++baseName; // jump over #
	while(*baseName == '_') // jump over all _
	{
		++baseName;
	}

	int oldIndex = atoi(baseName);
	int newIndex = oldIndex+1; // we increment by one

	// maximal size calculation (C99 would also allow non maximum though...)
	size_t sizeHash = 1;
	size_t sizeMax_ = 55;
	size_t sizeNum = 10;
	size_t size = sizeHash + sizeMax_ + sizeNum + 1;
	char newName[size]; // #_______________________________________________________4000000000

	// now we fill out newName
	size_t index = 0; // index of newName
	newName[index++] = '#';
	size_t size_=0;
	size_t i = newIndex/10;
	while (i>0)
	{
		size_++; // increment the number of decimals
		for (size_t j=0; j<size_; ++j)
		{
			newName[index++] = '_'; // index max. 56 for >1billion
		}
		i/=10;
	}
	if (snprintf (&newName[index], sizeNum, "%d", newIndex)  < 0)
	{
		return -1;
	}
	keySetBaseName(key, newName);

	/*
	if (!strncmp(keyBaseName(key), "#", 1)) // check if string starts with #
	{
		int newIndex = atoi(keyBaseName(key)+1 // parse old number
				)+1; // and increment 1
		if (newIndex > 9) // TODO: handle generation of _
		{
			return -1;
		}
		char str[3];
		if (snprintf (str, 3, "#%d", newIndex)  < 0)
		{
			return -1;
		}
		keySetBaseName(key, str);
	}
	else
	{
		return -1;
	}
	*/

	return 0;
}

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
			keySetBaseName (current, "#0");
			return 1;
		}
		else
		{
			// we are in an array
			Key * newKey = keyNew (keyName(current), KEY_END);
			keyArrayIncName(newKey);
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
			yajl_size_type stringLen)
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
			yajl_size_type stringLen)
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
			 yajl_size_type stringLen)
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
	keySetMeta(currentKey, "array", "");

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

	KeySet *config= elektraPluginGetConfig(handle);

	// ksClear (returned);
	if (!strncmp(keyName(parentKey), "user", 4))
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
#if YAJL_MAJOR == 1
	yajl_parser_config cfg = { 1, 1 };
	yajl_handle hand = yajl_alloc(&callbacks, &cfg, NULL, returned);
#else
	yajl_handle hand = yajl_alloc(&callbacks, NULL, returned);
	yajl_config(hand, yajl_allow_comments, 1);
#endif

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
		yajl_size_type rd = fread(	(void *) fileData, 1,
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
#if YAJL_MAJOR == 1
			stat = yajl_parse_complete(hand);
#else
			stat = yajl_complete_parse(hand);
#endif
		}
		else
		{
			stat = yajl_parse(hand, fileData, rd);
		}

		if (stat != yajl_status_ok
#if YAJL_MAJOR == 1
			&& stat != yajl_status_insufficient_data
#endif
			)
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

void elektraGenName(yajl_gen g, Key *cur)
{
	yajl_gen_string(g, (const unsigned char *)keyBaseName(cur), keyGetBaseNameSize(cur)-1);
}

char *keyNameGetOneLevel(const char *name, size_t *size); // defined in keyhelpers.c, API might be broken!

/**
 * @brief open so many levels as keys are different
 *
 * @pre cur and prev have a name which is not equal
 *
 * @param g handle to generate to
 * @param cur current key of iteration
 * @param prev previous key of iteration
 */
int elektraGenOpen(yajl_gen g, Key *cur, Key *prev)
{
	const char *p = keyName(cur);
	const char *x = keyName(prev);
	// search for first unequal character
	while(*p == *x)
	{
		++p;
		++x;
	}
	size_t size=0;
	int level = 0;

	// maximum needed buffer per element is the size of largest keyname
	char *buffer = malloc(keyGetNameSize(cur) > keyGetNameSize(prev) ?
			keyGetNameSize(cur)+1 :
			keyGetNameSize(prev)+1);

	while (*(p=keyNameGetOneLevel(p+size,&size)))
	{
		level ++;
		// copy what we found to a buffer, so we can NULL-terminate it
		strncpy(buffer,p,size);
		buffer[size]=0;

		printf("Open name: \"%s\"\n",buffer);
		yajl_gen_string(g, (const unsigned char *)buffer, size);
		yajl_gen_map_open(g);
	}

	return level;
}

int elektraGenClose(yajl_gen g, Key *cur, Key *prev)
{
	const char *p = keyName(cur);
	const char *x = keyName(prev);
	// search for first unequal character
	while(*p == *x)
	{
		++p;
		++x;
	}
	size_t size=0;
	int level = 0;

	// maximum needed buffer per element is the size of largest keyname
	char *buffer = malloc(keyGetNameSize(cur) > keyGetNameSize(prev) ?
			keyGetNameSize(cur)+1 :
			keyGetNameSize(prev)+1);

	x=keyNameGetOneLevel(x+size,&size); // skip first level to close, we assume that this was not a map
	while (*(x=keyNameGetOneLevel(x+size,&size)))
	{
		++ level;
		// copy what we found to a buffer, so we can NULL-terminate it
		strncpy(buffer,x,size);
		buffer[size]=0;

		printf("Close name: \"%s\"\n",buffer);
		yajl_gen_map_close(g);
	}
	return level;
}

int keyIsSibling(Key *cur, Key *prev)
{
	const char *p = keyName(cur);
	const char *x = keyName(prev);
	// search for first unequal character
	while(*p == *x)
	{
		++p;
		++x;
	}

	// now search if any of them has a / afterwards
	while(*p != 0)
	{
		if (*p == '/')
		{
			return 0;
		}
		++p;
	}
	while(*x != 0)
	{
		if (*x == '/')
		{
			return 0;
		}
		++x;
	}
	return 1; // they are siblings
}

int elektraYajlSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
#if YAJL_MAJOR == 1
	yajl_gen_config conf = { 1, "  " };
	yajl_gen g = yajl_gen_alloc(&conf, NULL
#else
	yajl_gen g = yajl_gen_alloc(NULL);
	yajl_gen_config(g, yajl_gen_beautify, 1);
	yajl_gen_config(g, yajl_gen_validate_utf8, 1);
#endif
	yajl_gen_map_open(g);

	Key *cur = 0;
	Key *prev = 0;
	int map_to_close = 1; // see above
	int in_array = 0; // if we are currently in an array

	ksRewind (returned);
	while ((cur = ksNext(returned)) != 0)
	{
		// skip root
		// TODO: make configureable deep ignoring
		if (!strcmp(keyName(cur), "user"))
		{
			continue;
		}
		else if (!strcmp(keyName(cur), "system"))
		{
			continue;
		}

		// TODO: open and close everywhere!

		if (in_array && !keyGetMeta(cur, "array")) // we leave an array
		{
			// printf ("leave array prev: %s to cur: %s\n", keyName(prev), keyName(cur));
			yajl_gen_array_close(g);
			in_array = 0;
		}
		else if (!in_array && keyGetMeta(cur, "array")) // we enter an array
		{
			// printf ("enter array prev: %s to cur: %s\n", keyName(prev), keyName(cur));
			elektraGenName(g, cur);
			yajl_gen_array_open(g);
			in_array = 1;
			continue;
		}

		const Key * type = keyGetMeta(cur, "type");
		if (!type && keyGetValueSize(cur) == 0) // empty binary type is null
		{
			if (!in_array) elektraGenName(g, cur);
			yajl_gen_null(g);
		}
		else if (!type && keyGetValueSize(cur) > 1)
		{
			map_to_close -= elektraGenClose(g, cur, prev);
			if (!in_array) elektraGenName(g, cur);
			yajl_gen_string(g, (const unsigned char *)keyString(cur), keyGetValueSize(cur)-1);
		}
		else if (!type) // not a string key, so it gives structure
		{
			if (!prev) // always create map for first key
			{
				elektraGenName(g, cur);
				yajl_gen_map_open(g);
				++map_to_close;
			}
			else if (keyIsBelow(prev, cur)) // generate map for other keys iff they are below
			{
				// printf ("open from prev: %s to cur: %s\n", keyName(prev), keyName(cur));
				map_to_close += elektraGenOpen(g, cur, prev);
			}
			else if (!keyIsSibling(prev, cur))
				// not below, not sibling, so we close down to level of one map and open up to map of other
			{
				// printf ("open and close from prev: %s to cur: %s\n", keyName(prev), keyName(cur));
				map_to_close -= elektraGenClose(g, cur, prev);
				map_to_close += elektraGenOpen(g, cur, prev);
			}
		}
		else if (!strcmp(keyString(type), "boolean"))
		{
			if (!strcmp(keyString(cur), "true"))
			{
				if (!in_array) elektraGenName(g, cur);
				yajl_gen_bool(g, 1);
			}
			else if (!strcmp(keyString(cur), "false"))
			{
				if (!in_array) elektraGenName(g, cur);
				yajl_gen_bool(g, 0);
			}
			else
			{
				ELEKTRA_ADD_WARNING(78, parentKey, "drop boolean which is neither true nor false");
			}
		}
		else if (!strcmp(keyString(type), "number"))
		{
			if (!in_array) elektraGenName(g, cur);
			yajl_gen_number(g, keyString(cur), keyGetValueSize(cur)-1);
		}
		else { // existing, but unknown or unsupported type, drop it and add warning
			ELEKTRA_ADD_WARNING(78, parentKey, keyString(type));
		}

		prev = cur;
	}

	if (in_array)
	{
		yajl_gen_array_close(g);
	}

	for (int i=0; i<map_to_close; ++i)
	{
		yajl_gen_map_close(g);
	}


	FILE *fp = fopen(keyString(parentKey), "w");
	if (!fp)
	{
		ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		return -1;
	}

	const unsigned char * buf;
	yajl_size_type len;
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


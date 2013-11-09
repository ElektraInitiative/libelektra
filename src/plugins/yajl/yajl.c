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
int elektraArrayIncName(Key *key)
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
	size_t sizeMax_ = 9; // maximum of n-1 _
	size_t sizeNum = 10; // maximum of 10 digits in 32bit number
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
		newName[index++] = '_'; // index n-1 of decimals
		i/=10;
	}
	if (snprintf (&newName[index], sizeNum, "%d", newIndex)  < 0)
	{
		return -1;
	}
	keySetBaseName(key, newName);

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

	if (keyGetMeta(current, "array")) // TODO: use # in name instead, metadata is avoidable
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
			// TODO use: ksAppendArray instead
			// we are in an array
			Key * newKey = keyNew (keyName(current), KEY_END);
			elektraArrayIncName(newKey);
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

int elektraKeyIsSibling(Key *cur, Key *prev)
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

char *keyNameGetOneLevel(const char *name, size_t *size); // defined in keyhelpers.c, API might be broken!

/**
 * @brief open so many levels as needed for key next
 *
 * Iterates over next and generate
 * needed groups for every / and needed arrays
 * for every name/#.
 * Yields name for leaf.
 *
 * TODO: cur should be renamed to prev and next should be renamed to cur
 *
 * @pre keys are not allowed to be below
 *
 * Example for elektraNextNotBelow:
 * cur:  user/sw/org
 * next: user/sw/org/deeper
 * -> do nothing, "deeper" is unknown
 *
 * cur:  user/sw/org/deeper
 * next: user/sw/org/other
 * -> this cannot happen (see elektraNextNotBelow)
 *
 * cur:  user/sw/org/other
 * next: user/sw/org/other/deeper/below
 * -> this cannot happen (see elektraNextNotBelow)
 *
 * @example
 *
 * instead of two entries above following would happen:
 * cur:  user/sw/org/deeper
 * next: user/sw/org/other/deeper/below
 * -> and "other" and "deeper" would be opened
 *
 * cur:  user/sw/org/other/deeper/below
 * next: user/no
 * -> do nothing, because "no" is value
 *
 * cur:  user/no
 * next: user/oops/it/is/below
 * -> create map "oops" "it" "is"
 *
 * cur:  user/oops/it/is/below
 * next: user/x/t/s/x
 * -> create "x" "t" "s"
 *
 * @example
 *
 * cur:  user/sw/org/#0/blah
 * next: user/sw/org/#1/test
 * -> will not open org or array (because that did not change),
 *    but will open group test (because within arrays every key
 *    needs a group).
 *
 * cur:  user/sw/org/#0/blah
 * next: user/sw/oth/#0/test
 * -> will open new group oth and new array and yield blah
 *
 *
 * @pre cur and next have a name which is not equal
 *
 * @param g handle to generate to
 * @param cur current key of iteration
 * @param next next key of iteration
 */
void elektraGenOpen(yajl_gen g, const Key *cur, const Key *next)
{
	const char *pcur = keyName(cur);
	const char *pend = pcur + keyGetNameSize(cur);
	const char *pnext = keyName(next);

	printf ("Open: pcur: %s , pnext: %s\n", pcur, pnext);

	// search for first unequal character
	while(*pnext == *pcur)
	{
		++pnext;
		++pcur;
	}

	size_t size=0;
	int group_open = 0;;
	while (*(pnext=keyNameGetOneLevel(pnext+size,&size)))
	{
		printf("Level: \"%.*s\"\n",(int)size, pnext);
		if (group_open)
		{
			printf ("open map because we stepped\n");
			yajl_gen_map_open(g);
			group_open = 0;
		}

		if (*pnext == '#') // we found an array in next
		{
			if (pcur<pend || *pcur != '#') // but the array is already open
			{
				yajl_gen_array_open(g);
			}
		}
		else  // it is an ordinary group
		{
			printf ("ordinary group\n");
			yajl_gen_string(g, (const unsigned char *)pnext, size);
			group_open = 1;
		}
	}
}

keyNameReverseIterator elektraKeyNameGetReverseIterator(const Key *k)
{
	keyNameReverseIterator it;
	it.rend   = keyName(k);
	it.rbegin = it.rend + keyGetNameSize(k);
	it.current = it.rbegin;
	it.size = 0;
	return it;
}


int elektraKeyNameReverseNext(keyNameReverseIterator *it)
{
	if (it->current == it->rend) // we are at the end (move that to hasNext?)
	{
		return 0;
	}

	const char *real=it->current-1; // start at one position left
	int endReached=0;

	// skip all repeating '/' in the "beginning" of string
	while (*real == KDB_PATH_SEPARATOR)
	{
		--real;
	}

	if (*real == KDB_PATH_ESCAPE)
	{
		++real; // we skipped to much
	}

	const char *currentEnd = real; // now we know where the string will end

	// now see where this basename begins
	// also handles escaped chars with '\'
	while (real != it->rend && !endReached)
	{
		--real;
		if (real != it->rend && *real==KDB_PATH_SEPARATOR)
		{
			// only decrement if we have not send the end
			--real;
			if (*real != KDB_PATH_ESCAPE)
			{
				endReached = 1;
				real += 2; // fix for lookahead
			}
		}
	}

	// update iterator and return it
	it->size=currentEnd-real+1;
	it->current = real;
	return 1;
}

// TODO: comment what this function does + how to test?
void elektraGenClose(yajl_gen g, const Key *cur, const Key *next)
{
	const char *x = keyName(cur);
	const char *p = keyName(next);
	const char *endp = p + keyGetNameSize(next);

	//TODO: search for last equal level
	//TODO: search for number of levels
	// search for first unequal character
	while(*p == *x)
	{
		++p;
		++x;
	}

	keyNameReverseIterator curIt =  elektraKeyNameGetReverseIterator(cur);
	// keyNameReverseIterator nextIt =  elektraKeyNameGetReverseIterator(next);
	while (elektraKeyNameReverseNext(&curIt))
	{
		printf("Close: \"%.*s\"\n", (int)curIt.size, curIt.current);
		if (*curIt.current == '#') // we found an array
		{
			if (p<endp || *p != '#') // and we won't be in the array next time
			{
				yajl_gen_array_close(g);
			}
		}
		else // it is an ordinary group
		{
			yajl_gen_map_close(g);
		}
	}
}

/**
 * @brief Generate the value for the current key
 *
 * No auto-guessing takes place, because that can be terrible wrong and
 * is not reversible. So make sure that all your boolean and numbers
 * have the proper type in meta value "type".
 *
 * @param g handle to generate to
 * @param parentKey needed for adding warnings/errors
 * @param cur the key to generate the value from
 */
void elektraGenValue(yajl_gen g, Key *parentKey, const Key *cur)
{
	printf ("Gen for %s\n", keyName(cur));

	const Key * type = keyGetMeta(cur, "type");
	if (!type && keyGetValueSize(cur) == 0) // empty binary type is null
	{
		yajl_gen_null(g);
	}
	else if (!type && keyGetValueSize(cur) >= 1) // default is string
	{
		yajl_gen_string(g, (const unsigned char *)keyString(cur), keyGetValueSize(cur)-1);
	}
	else if (!strcmp(keyString(type), "boolean"))
	{
		if (!strcmp(keyString(cur), "true"))
		{
			yajl_gen_bool(g, 1);
		}
		else if (!strcmp(keyString(cur), "false"))
		{
			yajl_gen_bool(g, 0);
		}
		else
		{
			ELEKTRA_ADD_WARNING(78, parentKey, "drop boolean which is neither true nor false");
		}
	}
	else if (!strcmp(keyString(type), "number")) // TODO: distuingish between float and int
	{
		yajl_gen_number(g, keyString(cur), keyGetValueSize(cur)-1);
	}
	else { // unknown or unsupported type, render it as string but add warning
		ELEKTRA_ADD_WARNING(78, parentKey, keyString(type));
		yajl_gen_string(g, (const unsigned char *)keyString(cur), keyGetValueSize(cur)-1);
	}
}


/**
 * @brief Forwards to key which is not below the next one
 *
 * Forwards at least forward one element.
 * ksCurrent() will point at the same key as the key which is returned.
 *
 * e.g.
 * user/sw/x
 * user/sw/x/y
 * user/sw/x/y/z1
 *
 * @retval last element if no other found.
 * @retval 0 if there is no other element afterwards (keyset will be
 * rewinded then)
 *
 * @param ks keyset to use
 *
 * @return key after starting position which is not below (to any latter
 * one)
 */
Key * elektraNextNotBelow(KeySet *ks)
{
	const Key *previous = ksNext(ks);

	if (!previous)
	{
		ksRewind(ks);
		return 0;
	}

	// unitialized variables are ok, because do{}while guarantees initialisation
	cursor_t pos; // always one before current
	const Key * current = previous; // current is same as ksCurrent()
	do
	{
		pos = ksGetCursor(ks); // remember candidate
		previous = current;    // and remember last key
		current = ksNext(ks);  // look forward to next key
	}
	while (current && keyIsBelow(previous, current));

	// jump to and return candidate, because next is known to be not
	// below candidate
	ksSetCursor(ks, pos);
	return ksCurrent(ks);
}

int elektraRemoveFile(Key *parentKey)
{
	FILE *fp = fopen(keyString(parentKey), "w"); // truncates file
	if (!fp)
	{
		ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
		return -1;
	}

	fclose (fp);
	return 0;
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

	ksRewind (returned);
	int first_key = 1;
	Key *cur = elektraNextNotBelow(returned);
	if (!cur)
	{
		return elektraRemoveFile(parentKey);
	}

	KeySet *config= elektraPluginGetConfig(handle);
	if (!strncmp(keyName(parentKey), "user", 4))
	{
		const Key * lookup = ksLookupByName(config, "/user_path", 0);
		if (!lookup)
		{
			elektraGenOpen(g, parentKey, cur);
		} else {
			elektraGenOpen(g, lookup, cur);
		}
	}
	else
	{
		const Key * lookup = ksLookupByName(config, "/system_path", 0);
		if (!lookup)
		{
			elektraGenOpen(g, parentKey, cur);
		} else {
			elektraGenOpen(g, lookup, cur);
		}
	}

	Key *next = 0;
	while ((next = elektraNextNotBelow(returned)) != 0)
	{
		printf ("in iter: %s next: %s\n", keyName(cur), keyName(next));

		printf ("in f: %s next: %s\n", keyName(cur), keyName(next));

		// for the first key we had not opened anything,
		// so we do not close anything
		if (!first_key)
		{
			elektraGenClose(g, cur, next);
		}
		else
		{
			first_key = 0;
		}

		elektraGenValue(g, parentKey, cur);
		elektraGenOpen(g, cur, next);

		cur = next;
	}

	// TODO close all affairs of last key

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


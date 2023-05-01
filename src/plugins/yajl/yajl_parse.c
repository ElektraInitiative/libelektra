/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./yajl.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <elektra/core/errors.h>
#include <elektra/ease/array.h>
#include <internal/macros/attributes.h>
#include <internal/macros/plugin_errors.h>
#include <yajl/yajl_parse.h>


static elektraCursor itKs;

static void elektraYajlSetArrayLength (KeySet * ks, Key * current)
{
	// Update array length in array key
	Key * arrayKey = keyNew (keyName (current), KEY_END);
	keySetBaseName (arrayKey, 0);
	Key * foundKey = ksLookup (ks, arrayKey, 0);
	keySetMeta (foundKey, "array", keyBaseName (current));
	keyDel (arrayKey);
}

/**
 @retval 0 if ksAtCursor(ks, it) does not hold an array entry
 @retval 1 if the array entry will be used because its the first
 @retval 2 if a new array entry was created
 @retval -1 error in snprintf
 */
static int elektraYajlIncrementArrayEntry (KeySet * ks)
{
	Key * current = ksAtCursor (ks, itKs);
	const char * baseName = keyBaseName (current);
	const char * meta = keyString (keyGetMeta (current, "array"));
	if (!strcmp (meta, "empty"))
	{
		current = keyNew (keyName (current), KEY_END);
		keyAddName (current, "#0");
		ksAppendKey (ks, current);
		itKs = ksSearch (ks, current);
		elektraYajlSetArrayLength (ks, current);

		return 1;
	}
	else if (baseName && *baseName == '#')
	{
		// we are in an array
		current = keyNew (keyName (current), KEY_END);
		elektraArrayIncName (current);
		ksAppendKey (ks, current);
		itKs = ksSearch (ks, current);
		elektraYajlSetArrayLength (ks, current);

		return 2;
	}
	else
	{
		// previous entry indicates this is not an array
		return 0;
	}
}

static int elektraYajlParseNull (void * ctx)
{
	KeySet * ks = (KeySet *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	Key * current = ksAtCursor (ks, itKs);

	keySetBinary (current, NULL, 0);

	ELEKTRA_LOG_DEBUG ("parse null");

	return 1;
}

static int elektraYajlParseBoolean (void * ctx, int boolean)
{
	KeySet * ks = (KeySet *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	Key * current = ksAtCursor (ks, itKs);

	if (boolean == 1)
	{
		keySetString (current, "1");
	}
	else
	{
		keySetString (current, "0");
	}
	keySetMeta (current, "type", "boolean");

	ELEKTRA_LOG_DEBUG ("%d", boolean);

	return 1;
}

static int elektraYajlParseNumber (void * ctx, const char * stringVal, yajl_size_type stringLen)
{
	KeySet * ks = (KeySet *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	Key * current = ksAtCursor (ks, itKs);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char *) stringVal;
	stringValue[stringLen] = '\0';

	ELEKTRA_LOG_DEBUG ("%s %zu", stringVal, stringLen);

	keySetString (current, stringVal);
	keySetMeta (current, "type", "double");

	// restore old character in buffer
	stringValue[stringLen] = delim;

	return 1;
}

static int elektraYajlParseString (void * ctx, const unsigned char * stringVal, yajl_size_type stringLen)
{
	KeySet * ks = (KeySet *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	Key * current = ksAtCursor (ks, itKs);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char *) stringVal;
	stringValue[stringLen] = '\0';

	ELEKTRA_LOG_DEBUG ("%s %zu", stringVal, stringLen);

	keySetString (current, stringValue);

	// restore old character in buffer
	stringValue[stringLen] = delim;
	return 1;
}

static int elektraYajlParseMapKey (void * ctx, const unsigned char * stringVal, yajl_size_type stringLen)
{
	KeySet * ks = (KeySet *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	Key * currentKey = keyNew (keyName (ksAtCursor (ks, itKs)), KEY_END);
	keySetString (currentKey, 0);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char *) stringVal;
	stringValue[stringLen] = '\0';

	ELEKTRA_LOG_DEBUG ("stringValue: %s currentKey: %s", stringValue, keyName (currentKey));
	if (currentKey && !strcmp (keyBaseName (currentKey), "___empty_map"))
	{
		// remove old key
		keyDel (ksLookup (ks, currentKey, KDB_O_POP));
		itKs = 0; // current key was popped from KeySet
		// now we know the name of the object
		keySetBaseName (currentKey, stringValue);
	}
	else
	{
		// we entered a new pair (inside the previous object)
		keySetBaseName (currentKey, stringValue);
	}
	ksAppendKey (ks, currentKey);
	itKs = ksSearch (ks, currentKey);

	// restore old character in buffer
	stringValue[stringLen] = delim;

	return 1;
}

static int elektraYajlParseStartMap (void * ctx)
{
	KeySet * ks = (KeySet *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	Key * currentKey = ksAtCursor (ks, itKs);

	Key * newKey = keyNew (keyName (currentKey), KEY_END);
	// add a pseudo element for empty map
	keyAddBaseName (newKey, "___empty_map");
	ksAppendKey (ks, newKey);
	itKs = ksSearch (ks, newKey);

	ELEKTRA_LOG_DEBUG ("with new key %s", keyName (newKey));

	return 1;
}

static int elektraYajlParseEnd (void * ctx)
{
	KeySet * ks = (KeySet *) ctx;
	Key * currentKey = ksAtCursor (ks, itKs);

	const char * meta = keyString (keyGetMeta (currentKey, "array"));
	// If array is still empty by the time we reach the end, replace with ""
	if (!strcmp (meta, "empty"))
	{
		keySetMeta (currentKey, "array", "");
		return 1;
	}

	Key * lookupKey = keyNew (keyName (currentKey), KEY_END);
	keySetBaseName (lookupKey, 0); // remove current baseName

	// lets move iterator to the correct place
	itKs = ksSearch (ks, lookupKey);

#ifdef HAVE_LOGGER
	if (itKs >= 0)
	{
		ELEKTRA_LOG_DEBUG ("Iterator position: %zd", itKs);
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("Did not find key %s", keyName (lookupKey));
	}
#endif

	keyDel (lookupKey);

	return 1;
}

static int elektraYajlParseStartArray (void * ctx)
{
	KeySet * ks = (KeySet *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	Key * currentKey = ksAtCursor (ks, itKs);

	Key * newKey = keyNew (keyName (currentKey), KEY_END);
	keySetMeta (newKey, "array", "empty");
	ksAppendKey (ks, newKey);
	itKs = ksSearch (ks, newKey);

	ELEKTRA_LOG_DEBUG ("with new key %s", keyName (newKey));

	return 1;
}

/**
 * @brief Remove all non-leaf keys except for arrays
 *
 * @param returned to remove the keys from
 */
static void elektraYajlParseSuppressNonLeafKeys (KeySet * returned)
{
	elektraCursor it = 0;
	Key * cur = ksAtCursor (returned, it);

	while (cur != NULL)
	{
		Key * next = ksAtCursor (returned, ++it);
		if (next == NULL) break;


		Key * peekDup = keyDup (next, KEY_CP_ALL);
		keySetBaseName (peekDup, 0);

		if (!strcmp (keyName (peekDup), keyName (cur)))
		{
			const char * baseName = keyBaseName (next);
			// TODO: Add test for empty array check
			if (strcmp (baseName, "#0"))
			{
				ELEKTRA_LOG_DEBUG ("Removing non-leaf key %s", keyName (cur));
				keyDel (ksLookup (returned, cur, KDB_O_POP));
				it--;
			}
			else
			{
				// Set array key to NULL to avoid empty ___dirdata entries
				keySetBinary (cur, NULL, 0);
			}
		}

		keyDel (peekDup);
		cur = ksAtCursor (returned, it);
	}
}

/**
 * @brief Remove ___empty_map if thats the only thing which would be
 *        returned.
 *
 * @param returned to remove the key from
 */
static void elektraYajlParseSuppressEmptyMap (KeySet * returned, Key * parentKey)
{

	if (ksGetSize (returned) == 2)
	{
		Key * lookupKey = keyDup (parentKey, KEY_CP_ALL);
		keyAddBaseName (lookupKey, "___empty_map");
		Key * toRemove = ksLookup (returned, lookupKey, KDB_O_POP);

#ifdef HAVE_LOGGER
		if (toRemove)
		{
			ELEKTRA_LOG_DEBUG ("remove %s", keyName (toRemove));
		}
		else
		{
			Key * cur;

			for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
			{
				cur = ksAtCursor (returned, it);
				ELEKTRA_LOG_DEBUG ("key %s has value %s", keyName (cur), keyString (cur));
			}

			ELEKTRA_LOG_DEBUG ("did not find %s", keyName (lookupKey));
		}
#endif
		if (toRemove)
		{
			keyDel (toRemove);
		}
		keyDel (lookupKey);
	}
}

static inline KeySet * elektraGetModuleConfig (void)
{
	return ksNew (30, keyNew ("system:/elektra/modules/yajl", KEY_VALUE, "yajl plugin waits for your orders", KEY_END),
		      keyNew ("system:/elektra/modules/yajl/exports", KEY_END),
		      keyNew ("system:/elektra/modules/yajl/exports/get", KEY_FUNC, elektraYajlGet, KEY_END),
		      keyNew ("system:/elektra/modules/yajl/exports/set", KEY_FUNC, elektraYajlSet, KEY_END),
#include "./readme_yajl.c"
		      keyNew ("system:/elektra/modules/yajl/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
		      keyNew ("system:/elektra/modules/yajl/config", KEY_END),
		      keyNew ("system:/elektra/modules/yajl/config/", KEY_VALUE, "system", KEY_END),
		      keyNew ("system:/elektra/modules/yajl/config/below", KEY_VALUE, "user", KEY_END),
		      keyNew ("system:/elektra/modules/yajl/config/needs/boolean/restoreas", KEY_VALUE, "none", KEY_END), KS_END);
}

int elektraYajlGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/yajl"))
	{
		KeySet * moduleConfig = elektraGetModuleConfig ();
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	yajl_callbacks callbacks = { elektraYajlParseNull,
				     elektraYajlParseBoolean,
				     NULL,
				     NULL,
				     elektraYajlParseNumber,
				     elektraYajlParseString,
				     elektraYajlParseStartMap,
				     elektraYajlParseMapKey,
				     elektraYajlParseEnd,
				     elektraYajlParseStartArray,
				     elektraYajlParseEnd };

	ksAppendKey (returned, keyNew (keyName ((parentKey)), KEY_END));

#if YAJL_MAJOR == 1
	yajl_parser_config cfg = { 1, 1 };
	yajl_handle hand = yajl_alloc (&callbacks, &cfg, NULL, returned);
#else
	yajl_handle hand = yajl_alloc (&callbacks, NULL, returned);
	yajl_config (hand, yajl_allow_comments, 1);
#endif

	int errnosave = errno;
	unsigned char fileData[65536];
	int done = 0;
	FILE * fileHandle = fopen (keyString (parentKey), "r");
	if (!fileHandle)
	{
		yajl_free (hand);
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	/* external iteration of the keyset */
	itKs = 0;
	while (!done)
	{
		yajl_size_type rd = fread ((void *) fileData, 1, sizeof (fileData) - 1, fileHandle);
		if (rd == 0)
		{
			if (!feof (fileHandle))
			{
				ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Error while reading file: %s", keyString (parentKey));
				fclose (fileHandle);
				yajl_free (hand);
				return -1;
			}
			done = 1;
		}
		fileData[rd] = 0;

		yajl_status stat;
		if (done)
		{
#if YAJL_MAJOR == 1
			stat = yajl_parse_complete (hand);
#else
			stat = yajl_complete_parse (hand);
#endif
		}
		else
		{
			stat = yajl_parse (hand, fileData, rd);
		}
		int test_status = (stat != yajl_status_ok);
#if YAJL_MAJOR == 1
		test_status = test_status && (stat != yajl_status_insufficient_data);
#endif
		if (test_status)
		{
			unsigned char * str = yajl_get_error (hand, 1, fileData, rd);
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Yajl parse error happened. Reason: %s", (char *) str);
			yajl_free_error (hand, str);
			yajl_free (hand);
			fclose (fileHandle);

			return -1;
		}
	}

	yajl_free (hand);
	fclose (fileHandle);
	elektraYajlParseSuppressNonLeafKeys (returned);
	elektraYajlParseSuppressEmptyMap (returned, parentKey);

	return 1; /* success */
}

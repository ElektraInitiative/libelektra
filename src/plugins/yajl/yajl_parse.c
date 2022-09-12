/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "yajl.h"

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <kdbease.h>
#include <kdberrors.h>
#include <kdbmacros.h>
#include <yajl/yajl_parse.h>


static void elektraYajlSetArrayLength (ElektraKeyset * ks, ElektraKey * current)
{
	// Update array length in array key
	elektraCursor cursor = elektraKeysetGetCursor (ks);
	ElektraKey * arrayKey = elektraKeyNew (elektraKeyName (current), ELEKTRA_KEY_END);
	elektraKeySetBaseName (arrayKey, 0);
	ElektraKey * foundKey = elektraKeysetLookup (ks, arrayKey, 0);
	elektraKeySetMeta (foundKey, "array", elektraKeyBaseName (current));
	elektraKeyDel (arrayKey);
	elektraKeysetSetCursor (ks, cursor);
}

/**
 @retval 0 if ksCurrent does not hold an array entry
 @retval 1 if the array entry will be used because its the first
 @retval 2 if a new array entry was created
 @retval -1 error in snprintf
 */
static int elektraYajlIncrementArrayEntry (ElektraKeyset * ks)
{
	ElektraKey * current = elektraKeysetCurrent (ks);
	const char * baseName = elektraKeyBaseName (current);
	const char * meta = elektraKeyString (elektraKeyGetMeta (current, "array"));
	if (!strcmp (meta, "empty"))
	{
		current = elektraKeyNew (elektraKeyName (current), ELEKTRA_KEY_END);
		elektraKeyAddName (current, "#0");
		elektraKeysetAppendKey (ks, current);

		elektraYajlSetArrayLength (ks, current);

		return 1;
	}
	else if (baseName && *baseName == '#')
	{
		// we are in an array
		current = elektraKeyNew (elektraKeyName (current), ELEKTRA_KEY_END);
		elektraArrayIncName (current);
		elektraKeysetAppendKey (ks, current);

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
	ElektraKeyset * ks = (ElektraKeyset *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	ElektraKey * current = elektraKeysetCurrent (ks);

	elektraKeySetBinary (current, NULL, 0);

	ELEKTRA_LOG_DEBUG ("parse null");

	return 1;
}

static int elektraYajlParseBoolean (void * ctx, int boolean)
{
	ElektraKeyset * ks = (ElektraKeyset *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	ElektraKey * current = elektraKeysetCurrent (ks);

	if (boolean == 1)
	{
		elektraKeySetString (current, "1");
	}
	else
	{
		elektraKeySetString (current, "0");
	}
	elektraKeySetMeta (current, "type", "boolean");

	ELEKTRA_LOG_DEBUG ("%d", boolean);

	return 1;
}

static int elektraYajlParseNumber (void * ctx, const char * stringVal, yajl_size_type stringLen)
{
	ElektraKeyset * ks = (ElektraKeyset *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	ElektraKey * current = elektraKeysetCurrent (ks);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char *) stringVal;
	stringValue[stringLen] = '\0';

	ELEKTRA_LOG_DEBUG ("%s %zu", stringVal, stringLen);

	elektraKeySetString (current, stringVal);
	elektraKeySetMeta (current, "type", "double");

	// restore old character in buffer
	stringValue[stringLen] = delim;

	return 1;
}

static int elektraYajlParseString (void * ctx, const unsigned char * stringVal, yajl_size_type stringLen)
{
	ElektraKeyset * ks = (ElektraKeyset *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	ElektraKey * current = elektraKeysetCurrent (ks);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char *) stringVal;
	stringValue[stringLen] = '\0';

	ELEKTRA_LOG_DEBUG ("%s %zu", stringVal, stringLen);

	elektraKeySetString (current, stringValue);

	// restore old character in buffer
	stringValue[stringLen] = delim;
	return 1;
}

static int elektraYajlParseMapKey (void * ctx, const unsigned char * stringVal, yajl_size_type stringLen)
{
	ElektraKeyset * ks = (ElektraKeyset *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	ElektraKey * currentKey = elektraKeyNew (elektraKeyName (elektraKeysetCurrent (ks)), ELEKTRA_KEY_END);
	elektraKeySetString (currentKey, 0);

	unsigned char delim = stringVal[stringLen];
	char * stringValue = (char *) stringVal;
	stringValue[stringLen] = '\0';

	ELEKTRA_LOG_DEBUG ("stringValue: %s currentKey: %s", stringValue, elektraKeyName (currentKey));
	if (currentKey && !strcmp (elektraKeyBaseName (currentKey), "___empty_map"))
	{
		// remove old key
		elektraKeyDel (elektraKeysetLookup (ks, currentKey, ELEKTRA_KDB_O_POP));
		// now we know the name of the object
		elektraKeySetBaseName (currentKey, stringValue);
	}
	else
	{
		// we entered a new pair (inside the previous object)
		elektraKeySetBaseName (currentKey, stringValue);
	}
	elektraKeysetAppendKey (ks, currentKey);

	// restore old character in buffer
	stringValue[stringLen] = delim;

	return 1;
}

static int elektraYajlParseStartMap (void * ctx)
{
	ElektraKeyset * ks = (ElektraKeyset *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	ElektraKey * currentKey = elektraKeysetCurrent (ks);

	ElektraKey * newKey = elektraKeyNew (elektraKeyName (currentKey), ELEKTRA_KEY_END);
	// add a pseudo element for empty map
	elektraKeyAddBaseName (newKey, "___empty_map");
	elektraKeysetAppendKey (ks, newKey);

	ELEKTRA_LOG_DEBUG ("with new key %s", elektraKeyName (newKey));

	return 1;
}

static int elektraYajlParseEnd (void * ctx)
{
	ElektraKeyset * ks = (ElektraKeyset *) ctx;
	ElektraKey * currentKey = elektraKeysetCurrent (ks);

	const char * meta = elektraKeyString (elektraKeyGetMeta (currentKey, "array"));
	// If array is still empty by the time we reach the end, replace with ""
	if (!strcmp (meta, "empty"))
	{
		elektraKeySetMeta (currentKey, "array", "");
		return 1;
	}

	ElektraKey * lookupKey = elektraKeyNew (elektraKeyName (currentKey), ELEKTRA_KEY_END);
	elektraKeySetBaseName (lookupKey, 0); // remove current baseName

	// lets point current to the correct place
	ElektraKey * foundKey = elektraKeysetLookup (ks, lookupKey, 0);

#ifdef HAVE_LOGGER
	if (foundKey)
	{
		ELEKTRA_LOG_DEBUG ("%s", elektraKeyName (foundKey));
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("did not find key %s", elektraKeyName (lookupKey));
	}
#else
	(void) foundKey; // foundKey is not used, but lookup is needed
#endif

	elektraKeyDel (lookupKey);

	return 1;
}

static int elektraYajlParseStartArray (void * ctx)
{
	ElektraKeyset * ks = (ElektraKeyset *) ctx;
	elektraYajlIncrementArrayEntry (ks);

	ElektraKey * currentKey = elektraKeysetCurrent (ks);

	ElektraKey * newKey = elektraKeyNew (elektraKeyName (currentKey), ELEKTRA_KEY_END);
	elektraKeySetMeta (newKey, "array", "empty");
	elektraKeysetAppendKey (ks, newKey);

	ELEKTRA_LOG_DEBUG ("with new key %s", elektraKeyName (newKey));

	return 1;
}

/**
 * @brief Remove all non-leaf keys except for arrays
 *
 * @param returned to remove the keys from
 */
static void elektraYajlParseSuppressNonLeafKeys (ElektraKeyset * returned)
{
	elektraKeysetRewind (returned);
	ElektraKey * cur = elektraKeysetNext (returned);
	while (cur != NULL)
	{
		elektraCursor cursor = elektraKeysetGetCursor (returned);

		if (elektraKeysetNext (returned) == NULL) break;

		ElektraKey * peekDup = elektraKeyDup (elektraKeysetCurrent (returned), ELEKTRA_KEY_CP_ALL);
		elektraKeySetBaseName (peekDup, 0);

		if (!strcmp (elektraKeyName (peekDup), elektraKeyName (cur)))
		{
			const char * baseName = elektraKeyBaseName (elektraKeysetCurrent (returned));
			// TODO: Add test for empty array check
			if (strcmp (baseName, "#0"))
			{
				ELEKTRA_LOG_DEBUG ("Removing non-leaf key %s", elektraKeyName (cur));
				elektraKeyDel (elektraKeysetLookup (returned, cur, ELEKTRA_KDB_O_POP));
				elektraKeysetSetCursor (returned, cursor);
			}
			else
			{
				// Set array key to NULL to avoid empty ___dirdata entries
				elektraKeySetBinary (cur, NULL, 0);
			}
		}

		elektraKeyDel (peekDup);
		cur = elektraKeysetCurrent (returned);
	}
}

/**
 * @brief Remove ___empty_map if thats the only thing which would be
 *        returned.
 *
 * @param returned to remove the key from
 */
static void elektraYajlParseSuppressEmptyMap (ElektraKeyset * returned, ElektraKey * parentKey)
{

	if (elektraKeysetGetSize (returned) == 2)
	{
		ElektraKey * lookupKey = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (lookupKey, "___empty_map");
		ElektraKey * toRemove = elektraKeysetLookup (returned, lookupKey, ELEKTRA_KDB_O_POP);

#ifdef HAVE_LOGGER
		if (toRemove)
		{
			ELEKTRA_LOG_DEBUG ("remove %s", elektraKeyName (toRemove));
		}
		else
		{
			elektraKeysetRewind (returned);
			ElektraKey * cur;
			while ((cur = elektraKeysetNext (returned)) != 0)
			{
				ELEKTRA_LOG_DEBUG ("key %s has value %s", elektraKeyName (cur), elektraKeyString (cur));
			}

			ELEKTRA_LOG_DEBUG ("did not find %s", elektraKeyName (lookupKey));
			elektraKeysetRewind (returned);
		}
#endif
		if (toRemove)
		{
			elektraKeyDel (toRemove);
		}
		elektraKeyDel (lookupKey);
	}
}

static inline ElektraKeyset * elektraGetModuleConfig (void)
{
	return elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/yajl", ELEKTRA_KEY_VALUE, "yajl plugin waits for your orders", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/yajl/exports", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/yajl/exports/get", ELEKTRA_KEY_FUNC, elektraYajlGet, ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/yajl/exports/set", ELEKTRA_KEY_FUNC, elektraYajlSet, ELEKTRA_KEY_END),
#include "readme_yajl.c"
		      elektraKeyNew ("system:/elektra/modules/yajl/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/yajl/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/yajl/config/", ELEKTRA_KEY_VALUE, "system", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/yajl/config/below", ELEKTRA_KEY_VALUE, "user", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/yajl/config/needs/boolean/restoreas", ELEKTRA_KEY_VALUE, "none", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

int elektraYajlGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/yajl"))
	{
		ElektraKeyset * moduleConfig = elektraGetModuleConfig ();
		elektraKeysetAppend (returned, moduleConfig);
		elektraKeysetDel (moduleConfig);
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

	elektraKeysetAppendKey (returned, elektraKeyNew (elektraKeyName ((parentKey)), ELEKTRA_KEY_END));

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
	FILE * fileHandle = fopen (elektraKeyString (parentKey), "r");
	if (!fileHandle)
	{
		yajl_free (hand);
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	while (!done)
	{
		yajl_size_type rd = fread ((void *) fileData, 1, sizeof (fileData) - 1, fileHandle);
		if (rd == 0)
		{
			if (!feof (fileHandle))
			{
				ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Error while reading file: %s", elektraKeyString (parentKey));
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

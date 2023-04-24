/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./yajl_gen.h"
#include <errno.h>
#include <internal/macros/plugin_errors.h>


/**
 * @brief Return the first character of next name level
 *
 * @pre it must be safe to look at pnext+size
 * @pre string must be null terminated
 *
 * @param pnext pointer to current level
 * @param size size of name in that level
 *
 * @return #lookahead
 */
lookahead_t elektraLookahead (const char * pnext, size_t size)
{
	lookahead_t lookahead = LOOKAHEAD_END; // found end
	if (*(pnext + size) == '/')
	{
		// we are not at end, so we can look one further
		if (strcmp (pnext + size + 1, "###empty_array") == 0)
		{
			lookahead = LOOKAHEAD_EMPTY_ARRAY;
		}
		else if (*(pnext + size + 1) == '#')
		{
			lookahead = LOOKAHEAD_ARRAY;
		}
		else
		{
			if (!strcmp (pnext + size + 1, "___empty_map"))
			{
				lookahead = LOOKAHEAD_EMPTY_MAP;
			}
			else
			{
				lookahead = LOOKAHEAD_MAP;
			}
		}
	}

	// / and not NULL for nice printing
	return lookahead; // found End
}

/**
 * @brief Implements special handling for last element of key name
 *
 * @pre g is in a map or array
 *
 * @post g need value as next step
 *
 * (L1)
 * #/_
 * If # is in the same counter stage, we just have to yield the name
 *
 * If # is in a new counter stage elektraGenOpenFirst already has
 * yield the map, so we also just have to yield the name
 *
 * (L2)
 * /#
 * does nothing (even for #/# the array was already done because of
 * lookahead)
 *
 * (L3)
 * /_
 * yields the name for the value
 *
 * @param g the generator
 * @param next the key
 * @retval 0 no value needed afterwards
 * @retval 1 value is needed
 */
static int elektraGenOpenValue (yajl_gen g, const Key * next)
{
	keyNameReverseIterator last = elektraKeyNameGetReverseIterator (next);
	elektraKeyNameReverseNext (&last);

	int valueNeeded = 1;

	ELEKTRA_LOG_DEBUG ("next: \"%.*s\"", (int) last.size, last.current);

	if (strcmp (last.current, "###empty_array") == 0)
	{
		ELEKTRA_LOG_DEBUG ("GEN empty array in value");
		yajl_gen_array_open (g);
		yajl_gen_array_close (g);
		valueNeeded = 0;
	}
	else if (!strcmp (last.current, "___empty_map"))
	{
		ELEKTRA_LOG_DEBUG ("GEN empty map in value");
		yajl_gen_map_open (g);
		yajl_gen_map_close (g);
		valueNeeded = 0;
	}
	else if (last.current[0] != '#')
	{
		ELEKTRA_LOG_DEBUG ("GEN string (L1,3)");
		yajl_gen_string (g, (const unsigned char *) last.current, last.size);
	}

	return valueNeeded;
}


/**
 * @brief Generate the value for the current key
 *
 * No auto-guessing takes place, because that can be terrible wrong and
 * is not reversible. So make sure that all your boolean and numbers
 * have the proper type in metavalue "type".
 *
 * In case of type problems it will be rendered as string but a warning
 * will be added. Use a type checker to avoid such problems.
 *
 * @param g handle to generate to
 * @param parentKey needed for adding warnings/errors
 * @param cur the key to generate the value from
 */
static bool elektraGenValue (yajl_gen g, Key * parentKey, Key * cur)
{
	if (strcmp (keyName (parentKey), keyName (cur)) && !elektraGenOpenValue (g, cur))
	{
		ELEKTRA_LOG_DEBUG ("Do not yield value");
		return true;
	}

	ELEKTRA_LOG_DEBUG ("GEN value %s for %s", keyString (cur), keyName (cur));

	const Key * type = keyGetMeta (cur, "type");
	if (!type && keyGetValueSize (cur) == 0) // empty binary type is null
	{
		yajl_gen_null (g);
	}
	else if ((!type && keyGetValueSize (cur) >= 1) || // default is string
		 (!strcmp (keyString (type), "string")))
	{
		yajl_gen_string (g, (const unsigned char *) keyString (cur), keyGetValueSize (cur) - 1);
	}
	else if (!strcmp (keyString (type), "boolean"))
	{
		if (!strcmp (keyString (cur), "1") || !strcmp (keyString (cur), "true"))
		{
			yajl_gen_bool (g, 1);
		}
		else if (!strcmp (keyString (cur), "0") || !strcmp (keyString (cur), "false"))
		{
			yajl_gen_bool (g, 0);
		}
		else
		{
			ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNING (parentKey, "Got boolean which is neither 1 or true nor 0 or false");
			yajl_gen_string (g, (const unsigned char *) keyString (cur), keyGetValueSize (cur) - 1);
		}
	}
	else if (!strcmp (keyString (type), "double"))
	{
		yajl_gen_number (g, keyString (cur), keyGetValueSize (cur) - 1);
	}
	else
	{ // unknown or unsupported type, render it as string but add warning
		ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey, "The key %s has unknown type: %s", keyName (cur), keyString (type));
		yajl_gen_string (g, (const unsigned char *) keyString (cur), keyGetValueSize (cur) - 1);
	}
	return true;
}

int elektraGenEmpty (yajl_gen g, KeySet * returned, Key * parentKey)
{
	int did_something = 0;
	// TODO: do all these situations actually occur?
	if (ksGetSize (returned) == 0) // we got nothing..
	{
		ELEKTRA_LOG_DEBUG ("GEN empty map (got nothing)");
		yajl_gen_map_open (g);
		yajl_gen_map_close (g);
		did_something = 1;
	}
	else if (ksGetSize (returned) == 1) // maybe just parentKey
	{
		if (!strcmp (keyName (ksAtCursor (returned, ksGetSize (returned) - 1)), keyName (parentKey)))
		{
			ELEKTRA_LOG_DEBUG ("GEN empty map (got parent)");
			yajl_gen_map_open (g);
			yajl_gen_map_close (g);
			did_something = 1;
		}
	}
	else if (ksGetSize (returned) == 2) // maybe just parent+specialkey
	{
		Key * toCheck = keyDup (parentKey, KEY_CP_ALL);

		keyAddBaseName (toCheck, "###empty_array");
		if (!strcmp (keyName (ksAtCursor (returned, ksGetSize (returned) - 1)), keyName (toCheck)))
		{
			ELEKTRA_LOG_DEBUG ("GEN empty array (got %s)", keyName (ksAtCursor (returned, ksGetSize (returned) - 1)));
			yajl_gen_array_open (g);
			yajl_gen_array_close (g);
			did_something = 1;
		}

		keySetBaseName (toCheck, "___empty_map");
		if (!strcmp (keyName (ksAtCursor (returned, ksGetSize (returned) - 1)), keyName (toCheck)))
		{
			ELEKTRA_LOG_DEBUG ("GEN empty map (got %s)", keyName (ksAtCursor (returned, ksGetSize (returned) - 1)));
			yajl_gen_map_open (g);
			yajl_gen_map_close (g);
			did_something = 1;
		}
		keyDel (toCheck);
	}

	return did_something;
}

int elektraGenWriteFile (yajl_gen g, Key * parentKey)
{
	int errnosave = errno;
	FILE * fp = fopen (keyString (parentKey), "w");

	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	const unsigned char * buf;
	yajl_size_type len;
	yajl_gen_get_buf (g, &buf, &len);
	fwrite (buf, 1, len, fp);
	yajl_gen_clear (g);

	fclose (fp);

	errno = errnosave;
	return 1; /* success */
}

static void elektraCheckForEmptyArray (KeySet * ks)
{
	Key * cur = 0;

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		cur = ksAtCursor (ks, it);
		ELEKTRA_LOG_DEBUG ("WALK: %s", keyName (cur));
		const char * meta = keyString (keyGetMeta (cur, "array"));
		if (*meta == '\0')
		{
			Key * k = keyNew (keyName (cur), KEY_END);
			keyAddBaseName (k, "###empty_array");

			ELEKTRA_LOG_DEBUG ("Add empty array: %s", keyName (k));

			ksAppendKey (ks, k);
		}
	}
}

static bool elektraCheckForInvalidMetaKey (Key * parentKey, KeySet * ks)
{
	Key * cur = 0;
	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		cur = ksAtCursor (ks, it);
		const KeySet * metaKeys = keyMeta (cur);
		for (elektraCursor jt = 0; jt < ksGetSize (metaKeys); ++jt)
		{
			const Key * meta = ksAtCursor (metaKeys, jt);
			const char * pos = (const char *) keyName (meta);
			if (elektraStrCmp (pos, "meta:/type") != 0 && elektraStrCmp (pos, "meta:/array") != 0 &&
			    elektraStrCmp (pos, "meta:/binary") != 0)
			{
				ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "The Metakey %s is not supported by yajl", keyName (meta));
				return false;
			}
		}
	}
	return true;
}

int elektraYajlSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
#if YAJL_MAJOR == 1
	yajl_gen_config conf = { 1, "    " };
	yajl_gen g = yajl_gen_alloc (&conf, NULL);
#else
	yajl_gen g = yajl_gen_alloc (NULL);
	yajl_gen_config (g, yajl_gen_beautify, 1);
#endif

	if (!elektraCheckForInvalidMetaKey (parentKey, returned))
	{
		yajl_gen_free (g);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	elektraCheckForEmptyArray (returned);

	if (ksGetSize (returned) == 1 && !strcmp (keyName (parentKey), keyName (ksAtCursor (returned, 0))) &&
	    keyGetValueSize (ksAtCursor (returned, 0)) > 1)
	{
		if (!elektraGenValue (g, parentKey, ksAtCursor (returned, 0)))
		{
			return -1;
		}
		int ret = elektraGenWriteFile (g, parentKey);
		yajl_gen_free (g);
		return ret;
	}

	if (elektraGenEmpty (g, returned, parentKey))
	{
		int ret = elektraGenWriteFile (g, parentKey);
		yajl_gen_free (g);
		return ret;
	}

	Key * cur = elektraNextNotBelow (returned, 0);
	if (!cur)
	{
		// empty config should be handled by resolver
		// (e.g. remove file)
		yajl_gen_free (g);
		return 0;
	}

	ELEKTRA_LOG_DEBUG ("parentKey: %s, cur: %s", keyName (parentKey), keyName (cur));
	elektraGenOpenInitial (g, parentKey, cur);


	Key * next = 0;
	elektraCursor it = ksSearch (returned, cur) + 1;
	while ((next = elektraNextNotBelow (returned, it)) != 0)
	{
		elektraGenValue (g, parentKey, cur);
		elektraGenClose (g, cur, next);

		ELEKTRA_LOG_DEBUG ("ITERATE: %s next: %s", keyName (cur), keyName (next));
		elektraGenOpen (g, cur, next);

		cur = next;
		it = ksSearch (returned, cur) + 1;
	}

	ELEKTRA_LOG_DEBUG ("leaving loop: %s", keyName (cur));

	elektraGenValue (g, parentKey, cur);

	elektraGenCloseFinally (g, cur, parentKey);

	int ret = elektraGenWriteFile (g, parentKey);
	yajl_gen_free (g);

	return ret;
}

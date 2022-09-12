/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "yajl_gen.h"
#include <errno.h>


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
static int elektraGenOpenValue (yajl_gen g, const ElektraKey * next)
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
static void elektraGenValue (yajl_gen g, ElektraKey * parentKey, const ElektraKey * cur)
{
	if (strcmp (elektraKeyName (parentKey), elektraKeyName (cur)) && !elektraGenOpenValue (g, cur))
	{
		ELEKTRA_LOG_DEBUG ("Do not yield value");
		return;
	}

	ELEKTRA_LOG_DEBUG ("GEN value %s for %s", elektraKeyString (cur), elektraKeyName (cur));

	const ElektraKey * type = elektraKeyGetMeta (cur, "type");
	if (!type && elektraKeyGetValueSize (cur) == 0) // empty binary type is null
	{
		yajl_gen_null (g);
	}
	else if ((!type && elektraKeyGetValueSize (cur) >= 1) || // default is string
		 (!strcmp (elektraKeyString (type), "string")))
	{
		yajl_gen_string (g, (const unsigned char *) elektraKeyString (cur), elektraKeyGetValueSize (cur) - 1);
	}
	else if (!strcmp (elektraKeyString (type), "boolean"))
	{
		if (!strcmp (elektraKeyString (cur), "1") || !strcmp (elektraKeyString (cur), "true"))
		{
			yajl_gen_bool (g, 1);
		}
		else if (!strcmp (elektraKeyString (cur), "0") || !strcmp (elektraKeyString (cur), "false"))
		{
			yajl_gen_bool (g, 0);
		}
		else
		{
			ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNING (parentKey, "Got boolean which is neither 1 or true nor 0 or false");
			yajl_gen_string (g, (const unsigned char *) elektraKeyString (cur), elektraKeyGetValueSize (cur) - 1);
		}
	}
	else if (!strcmp (elektraKeyString (type), "double"))
	{
		yajl_gen_number (g, elektraKeyString (cur), elektraKeyGetValueSize (cur) - 1);
	}
	else
	{ // unknown or unsupported type, render it as string but add warning
		ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey, "The key %s has unknown type: %s", elektraKeyName (cur), elektraKeyString (type));
		yajl_gen_string (g, (const unsigned char *) elektraKeyString (cur), elektraKeyGetValueSize (cur) - 1);
	}
}

int elektraGenEmpty (yajl_gen g, ElektraKeyset * returned, ElektraKey * parentKey)
{
	int did_something = 0;
	// TODO: do all these situations actually occur?
	if (elektraKeysetGetSize (returned) == 0) // we got nothing..
	{
		ELEKTRA_LOG_DEBUG ("GEN empty map (got nothing)");
		yajl_gen_map_open (g);
		yajl_gen_map_close (g);
		did_something = 1;
	}
	else if (elektraKeysetGetSize (returned) == 1) // maybe just parentKey
	{
		if (!strcmp (elektraKeyName (elektraKeysetTail (returned)), elektraKeyName (parentKey)))
		{
			ELEKTRA_LOG_DEBUG ("GEN empty map (got parent)");
			yajl_gen_map_open (g);
			yajl_gen_map_close (g);
			did_something = 1;
		}
	}
	else if (elektraKeysetGetSize (returned) == 2) // maybe just parent+specialkey
	{
		ElektraKey * toCheck = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);

		elektraKeyAddBaseName (toCheck, "###empty_array");
		if (!strcmp (elektraKeyName (elektraKeysetTail (returned)), elektraKeyName (toCheck)))
		{
			ELEKTRA_LOG_DEBUG ("GEN empty array (got %s)", elektraKeyName (elektraKeysetTail (returned)));
			yajl_gen_array_open (g);
			yajl_gen_array_close (g);
			did_something = 1;
		}

		elektraKeySetBaseName (toCheck, "___empty_map");
		if (!strcmp (elektraKeyName (elektraKeysetTail (returned)), elektraKeyName (toCheck)))
		{
			ELEKTRA_LOG_DEBUG ("GEN empty map (got %s)", elektraKeyName (elektraKeysetTail (returned)));
			yajl_gen_map_open (g);
			yajl_gen_map_close (g);
			did_something = 1;
		}
		elektraKeyDel (toCheck);
	}

	return did_something;
}

int elektraGenWriteFile (yajl_gen g, ElektraKey * parentKey)
{
	int errnosave = errno;
	FILE * fp = fopen (elektraKeyString (parentKey), "w");

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

static void elektraCheckForEmptyArray (ElektraKeyset * ks)
{
	ElektraKey * curr = 0;
	elektraKeysetRewind (ks);

	while ((curr = elektraKeysetNext (ks)) != 0)
	{
		ELEKTRA_LOG_DEBUG ("WALK: %s", elektraKeyName (curr));
		const char * meta = elektraKeyString (elektraKeyGetMeta (curr, "array"));
		if (*meta == '\0')
		{
			elektraCursor cursor = elektraKeysetGetCursor (ks);

			ElektraKey * k = elektraKeyNew (elektraKeyName (curr), ELEKTRA_KEY_END);
			elektraKeyAddBaseName (k, "###empty_array");

			ELEKTRA_LOG_DEBUG ("Add empty array: %s", elektraKeyName (k));

			elektraKeysetAppendKey (ks, k);

			elektraKeysetSetCursor (ks, cursor);
		}
	}
}

int elektraYajlSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
#if YAJL_MAJOR == 1
	yajl_gen_config conf = { 1, "    " };
	yajl_gen g = yajl_gen_alloc (&conf, NULL);
#else
	yajl_gen g = yajl_gen_alloc (NULL);
	yajl_gen_config (g, yajl_gen_beautify, 1);
#endif

	elektraCheckForEmptyArray (returned);

	if (elektraKeysetGetSize (returned) == 1 && !strcmp (elektraKeyName (parentKey), elektraKeyName (elektraKeysetHead (returned))) &&
	    elektraKeyGetValueSize (elektraKeysetHead (returned)) > 1)
	{
		elektraGenValue (g, parentKey, elektraKeysetHead (returned));
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

	elektraKeysetRewind (returned);
	ElektraKey * cur = elektraNextNotBelow (returned);
	if (!cur)
	{
		// empty config should be handled by resolver
		// (e.g. remove file)
		yajl_gen_free (g);
		return 0;
	}

	ELEKTRA_LOG_DEBUG ("parentKey: %s, cur: %s", elektraKeyName (parentKey), elektraKeyName (cur));
	elektraGenOpenInitial (g, parentKey, cur);

	ElektraKey * next = 0;
	while ((next = elektraNextNotBelow (returned)) != 0)
	{
		elektraGenValue (g, parentKey, cur);
		elektraGenClose (g, cur, next);

		ELEKTRA_LOG_DEBUG ("ITERATE: %s next: %s", elektraKeyName (cur), elektraKeyName (next));
		elektraGenOpen (g, cur, next);

		cur = next;
	}

	ELEKTRA_LOG_DEBUG ("leaving loop: %s", elektraKeyName (cur));

	elektraGenValue (g, parentKey, cur);

	elektraGenCloseFinally (g, cur, parentKey);

	int ret = elektraGenWriteFile (g, parentKey);
	yajl_gen_free (g);

	return ret;
}

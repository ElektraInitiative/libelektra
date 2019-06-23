/**
 * @file
 *
 * @brief Source for enum plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "enum.h"
#include <ctype.h>
#include <kdberrors.h>
#include <kdbmeta.h>
#include <kdbutility.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>


static int validateKey (Key *, Key *);

// turn list into null-terminated array. strip quotes from elements and
// drop brackets from the beginning and end of the list

static char ** stringToArray (const char * string, const char * delim)
{
	if (!string || !delim) return NULL;

	char ** array = NULL;
	int elems = 1;
	const char * ptr = string;

	while (*ptr)
	{
		if (*ptr == *delim)
		{
			++elems;
		}
		++ptr;
	}
	++elems; // terminating element
	array = elektraCalloc (elems * sizeof (char *));
	char * localString = NULL;
	if (string[0] == '[' || string[0] == '(' || string[0] == '{')
		localString = elektraStrDup (string + 1);
	else
		localString = elektraStrDup (string);
	char lastChar = localString[strlen (localString) - 1];
	if (lastChar == ']' || lastChar == ')' || lastChar == '}') localString[strlen (localString) - 1] = '\0';

	int current = 0;
	ptr = strtok (localString, delim);
	while (ptr)
	{
		if (*ptr == '\0')
		{
			ptr = strtok (NULL, delim);
			continue;
		}
		char * tmp = elektraStrDup (ptr);
		char * start = elektraLskip (tmp);
		(void) elektraRstrip (start, NULL);
		if (start[0] == '\'') ++start;
		if (start[strlen (start) - 1] == '\'') start[strlen (start) - 1] = '\0';
		array[current++] = elektraStrDup (start);
		elektraFree (tmp);
		ptr = strtok (NULL, delim);
	}
	if (current + 1 < elems)
	{
		elektraRealloc ((void **) array, (current + 1) * (sizeof (char *)));
		array[current] = NULL;
	}
	elektraFree (localString);
	return array;
}

static void freeArray (char ** array)
{
	for (size_t i = 0; array[i] != NULL; ++i)
	{
		elektraFree (array[i]);
	}
	elektraFree (array);
}

static int validateWithList (Key * key, const char * meta)
{
	const char * validValues = keyString (keyGetMeta (key, meta));
	const Key * multiEnum = keyGetMeta (key, "check/enum/multi");
	char ** list = stringToArray (validValues, ",");
	if (!list) return 0;

	if (!multiEnum)
	{
		const char * value = keyString (key);
		for (size_t i = 0; list[i] != NULL; ++i)
		{
			if (!strcmp (list[i], value))
			{
				freeArray (list);
				return 1;
			}
		}
	}
	else
	{
		char * delim = (char *) keyString (multiEnum);
		char ** array = stringToArray (keyString (key), delim);
		if (!array)
		{
			freeArray (list);
			return 0;
		}
		for (size_t i = 0; array[i] != NULL; ++i)
		{
			int found = 0;
			for (size_t j = 0; list[j] != NULL; ++j)
			{
				if (!strcmp (array[i], list[j]))
				{
					found = 1;
					break;
				}
			}
			if (!found)
			{
				freeArray (array);
				freeArray (list);
				return 0;
			}
		}
		freeArray (list);
		freeArray (array);
		return 1;
	}
	freeArray (list);
	return 0;
}

static int validateWithArray (Key * key)
{
	const Key * multiEnum = keyGetMeta (key, "check/enum/multi");
	KeySet * validValues = elektraMetaArrayToKS (key, "check/enum");

	Key * cur;
	// got to first element
	ksNext (validValues);
	cur = ksNext (validValues);
	int rc = 0;
	if (cur)
	{
		if (multiEnum)
		{
			char * delim = (char *) keyString (multiEnum);
			char ** array = stringToArray (keyString (key), delim);
			if (!array)
			{
				goto VALIDATE_END;
			}
			for (size_t i = 0; array[i] != NULL; ++i)
			{
				int found = 0;
				ksRewind (validValues);
				while ((cur = ksNext (validValues)) != NULL)
				{
					if (!strcmp (array[i], keyString (cur)))
					{
						keyDel (ksLookup (validValues, cur, KDB_O_POP));
						found = 1;
						break;
					}
				}
				if (!found)
				{
					freeArray (array);
					goto VALIDATE_END;
				}
			}
			freeArray (array);
			rc = 1;
			goto VALIDATE_END;
		}
		else
		{
			ksRewind (validValues);
			while ((cur = ksNext (validValues)) != NULL)
			{
				if (!strcmp (keyString (key), keyString (cur)))
				{
					rc = 1;
					goto VALIDATE_END;
				}
			}
		}
	}
VALIDATE_END:
	ksDel (validValues);
	return rc;
}

static int validateKey (Key * key, Key * parentKey)
{
	int rc = 0;
	const Key * meta = keyGetMeta (key, "check/enum");
	if (keyString (meta)[0] != '#')
		rc = validateWithList (key, "check/enum");
	else
		rc = validateWithArray (key);
	if (!rc)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Validation of key '%s' with string '%s' failed", keyName (key),
							keyString (key));
	}
	return rc;
}

int elektraEnumGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/enum"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/enum", KEY_VALUE, "enum plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/enum/exports", KEY_END),
			       keyNew ("system/elektra/modules/enum/exports/get", KEY_FUNC, elektraEnumGet, KEY_END),
			       keyNew ("system/elektra/modules/enum/exports/set", KEY_FUNC, elektraEnumSet, KEY_END),
			       keyNew ("system/elektra/modules/enum/exports/validateKey", KEY_FUNC, validateKey, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/enum/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */
	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/enum");
		if (!meta) continue;
		if (!validateKey (cur, parentKey))
		{
			return -1;
		}
	}
	return 1; /* success */
}

int elektraEnumSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */
	Key * cur;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * meta = keyGetMeta (cur, "check/enum");
		if (!meta) continue;
		if (!validateKey (cur, parentKey))
		{
			return -1;
		}
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("enum", 
            ELEKTRA_PLUGIN_GET, 	&elektraEnumGet,
            ELEKTRA_PLUGIN_SET, 	&elektraEnumSet,
            ELEKTRA_PLUGIN_END);
}

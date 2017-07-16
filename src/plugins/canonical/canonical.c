/**
 * @file
 *
 * @brief Source for canonical plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "canonical.h"

#include <fnmatch.h>
#include <kdbease.h> //elektraReadArrayNumber
#include <kdbhelper.h>
#include <kdbmeta.h>    //elektraMetaArrayToKS
#include <kdbos.h>      //ELEKTRA_MAX_ARRAY_SIZE
#include <kdbprivate.h> //elektraArrayValidateName
#include <kdbtypes.h>   //elektra_long_long_t
#include <kdbutility.h> //elektraLskip, elektraRstrip
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h> //strcasecmp


typedef enum {
	REGEX = (1 << 0),
	FNMATCH = (1 << 1),
	LISTSENSITIVE = (1 << 2),
	LISTINSENSITIVE = (1 << 3),
	WORD = (1 << 4),
} TransformType;

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
		(void)elektraRstrip (start, NULL);
		if (start[0] == '\'') ++start;
		if (start[strlen (start) - 1] == '\'') start[strlen (start) - 1] = '\0';
		array[current++] = elektraStrDup (start);
		elektraFree (tmp);
		ptr = strtok (NULL, delim);
	}
	if (current + 1 < elems)
	{
		elektraRealloc ((void **)array, (current + 1) * (sizeof (char *)));
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

// if meta has a child-key 'canonical' override value of key with
// the value of <meta>/canonical
// else
// if meta is a valid array member, set the value of key to metas index
static int doTransform (Key * key, const Key * meta)
{
	Key * searchKey = keyNew (keyName (meta), KEY_META_NAME, KEY_END);
	keyAddBaseName (searchKey, "set");
	const Key * canonicalMeta = keyGetMeta (key, keyName (searchKey));
	keyDel (searchKey);
	if (canonicalMeta)
	{
		const char * canonicalValue = keyString (canonicalMeta);
		keySetMeta (key, "internal/canonical/origvalue", keyString (key));
		keySetString (key, canonicalValue);
	}
	else if (elektraArrayValidateName (meta) == 1)
	{
		kdb_long_long_t index = 0;
		const char * baseName = keyBaseName (meta);
		++baseName;		 // skip #
		while (*baseName == '_') // skip _
			++baseName;
		if (!elektraReadArrayNumber (baseName, &index))
		{
			char buffer[ELEKTRA_MAX_ARRAY_SIZE] = { 0 };
			snprintf (buffer, sizeof (buffer), "%lld", (long long)index);
			keySetMeta (key, "internal/canonical/origvalue", keyString (key));
			keySetString (key, buffer);
		}
	}
	return 1;
}

static int canonicalRegexSingle (Key * key, const Key * meta)
{
	const char * value = keyString (key);
	const char * regexString = strchr (keyString (meta), ':') + 1;

	regex_t regex;
	int ret = regcomp (&regex, regexString, REG_EXTENDED);

	if (ret != 0)
	{
		char buffer[1000];
		regerror (ret, &regex, buffer, 999);
		regfree (&regex);
		return -1;
	}
	int nomatch = 0;
	nomatch = regexec (&regex, value, 0, NULL, 0);
	regfree (&regex);
	if (!nomatch)
	{
		return doTransform (key, meta);
	}
	return 0;
}

static int canonicalFNMatchSingle (Key * key, const Key * meta)
{
	const char * value = keyString (key);
	const char * pattern = strchr (keyString (meta), ':') + 1;
	if (!fnmatch (pattern, value, 0))
	{
		return doTransform (key, meta);
	}
	return 0;
}

static int canonicalListSingle (Key * key, const Key * meta, int (*cmpFun) (const char *, const char *))
{
	const char * value = keyString (key);
	char ** list = stringToArray (strchr (keyString (meta), ':') + 1, ",");
	if (!list) return -1;
	int rc = 0;
	for (size_t i = 0; list[i] != NULL; ++i)
	{
		if (!cmpFun (value, list[i]))
		{
			rc = doTransform (key, meta);
			break;
		}
	}
	freeArray (list);
	return rc;
}

static int canonicalWord (Key * key, const Key * meta)
{
	if (!strcmp (keyString (key), keyString (meta)))
	{
		return doTransform (key, meta);
	}
	return 0;
}

static TransformType getType (const char * string)
{
	if (!strncasecmp (string, "regex:", sizeof ("regex:") - 1))
		return REGEX;
	else if (!strncasecmp (string, "fnm:", sizeof ("fnm:") - 1))
		return FNMATCH;
	else if (!strncasecmp (string, "caselist:", sizeof ("caselist:") - 1))
		return LISTINSENSITIVE;
	else if (!strncasecmp (string, "list:", sizeof ("list:") - 1))
		return LISTSENSITIVE;
	else
		return WORD;
}

static int transformSingle (Key * key, const Key * meta)
{
	TransformType type = getType (keyString (meta));
	int rc = 0;
	switch (type)
	{
	case REGEX:
		rc = canonicalRegexSingle (key, meta);
		break;
	case FNMATCH:
		rc = canonicalFNMatchSingle (key, meta);
		break;
	case LISTSENSITIVE:
		rc = canonicalListSingle (key, meta, strcmp);
		break;
	case LISTINSENSITIVE:
		rc = canonicalListSingle (key, meta, strcasecmp);
		break;
	case WORD:
		rc = canonicalWord (key, meta);
		break;
	default:
		rc = 0;
		break;
	}
	return rc;
}

static int transformArray (Key * key, const Key * meta)
{
	KeySet * lists = elektraMetaArrayToKS (key, keyName (meta));
	Key * elem = NULL;
	int rc = 0;
	while ((elem = ksNext (lists)) != NULL)
	{
		if (!strcmp (keyName (elem), "transform/canonical")) continue;
		rc = transformSingle (key, elem);
		if (rc == 1) break;
	}
	ksDel (lists);
	return rc;
}

static int transformKey (Key * key)
{
	const Key * meta = NULL;
	meta = keyGetMeta (key, "transform/canonical");
	if (!meta) return 0;
	if (keyGetMeta (key, "internal/canonical/origvalue")) return 0;
	int rc = 0;
	if (keyString (meta)[0] == '#')
	{
		rc = transformArray (key, meta);
	}
	else
	{
		rc = transformSingle (key, meta);
	}
	return rc;
}

static void restoreKey (Key * key)
{
	const Key * origKey = keyGetMeta (key, "internal/canonical/origvalue");
	if (origKey)
	{
		keySetString (key, keyString (origKey));
		keySetMeta (key, keyName (origKey), 0);
	}
}

int elektraCanonicalGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/canonical"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/canonical", KEY_VALUE, "canonical plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/canonical/exports", KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/get", KEY_FUNC, elektraCanonicalGet, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/set", KEY_FUNC, elektraCanonicalSet, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/checkconf", KEY_FUNC, elektraCanonicalCheckConfig, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/transformKey", KEY_FUNC, transformKey, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/restoreKey", KEY_FUNC, restoreKey, KEY_END),
#include ELEKTRA_README (canonical)
			keyNew ("system/elektra/modules/canonical/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	Key * cur = NULL;
	int ELEKTRA_UNUSED rc = 0;
	while ((cur = ksNext (returned)) != NULL)
	{
		rc = transformKey (cur);
	}

	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraCanonicalSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	Key * cur = NULL;
	while ((cur = ksNext (returned)) != NULL)
	{
		restoreKey (cur);
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraCanonicalCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (canonical)
{
	// clang-format off
    return elektraPluginExport ("canonical",
            ELEKTRA_PLUGIN_GET,	&elektraCanonicalGet,
            ELEKTRA_PLUGIN_SET,	&elektraCanonicalSet,
            ELEKTRA_PLUGIN_END);
}

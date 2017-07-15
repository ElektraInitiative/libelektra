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

int elektraCanonicalOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCanonicalClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

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
	keyAddBaseName (searchKey, "canonical");
	const Key * canonicalMeta = keyGetMeta (key, keyName (searchKey));
	keyDel (searchKey);
	if (canonicalMeta)
	{
		const char * canonicalValue = keyString (canonicalMeta);
		keySetMeta (key, "transform/canonical/origvalue", keyString (key));
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
			keySetMeta (key, "transform/canonical/origvalue", keyString (key));
			keySetString (key, buffer);
		}
	}
	return 1;
}

static int canonicalRegexSingle (Key * key, const Key * meta)
{
	const char * value = keyString (key);
	const char * regexString = keyString (meta);

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

static int canonicalRegexArray (Key * key, const Key * meta)
{
	KeySet * lists = elektraMetaArrayToKS (key, keyName (meta));
	Key * elem = NULL;
	int rc = 0;
	while ((elem = ksNext (lists)) != NULL)
	{
		rc = canonicalRegexSingle (key, elem);
		if (rc != 0) break;
	}
	ksDel (lists);
	return rc;
}


static int canonicalRegex (Key * key, const Key * meta)
{
	int rc = 0;
	if (keyString (meta)[0] == '#')
		rc = canonicalRegexArray (key, meta);
	else
		rc = canonicalRegexSingle (key, meta);

	return rc;
}

static int canonicalFNMatchSingle (Key * key, const Key * meta)
{
	const char * value = keyString (key);
	const char * pattern = keyString (meta);
	if (!fnmatch (pattern, value, 0))
	{
		return doTransform (key, meta);
	}
	return 0;
}

static int canonicalFNMatchArray (Key * key, const Key * meta)
{
	KeySet * lists = elektraMetaArrayToKS (key, keyName (meta));
	Key * elem = NULL;
	int rc = 0;
	while ((elem = ksNext (lists)) != NULL)
	{
		rc = canonicalFNMatchSingle (key, elem);
		if (rc != 0) break;
	}
	ksDel (lists);
	return rc;
}

static int canonicalFNMatch (Key * key, const Key * meta)
{
	int rc = 0;
	if (keyString (meta)[0] == '#')
		rc = canonicalFNMatchArray (key, meta);
	else
		rc = canonicalFNMatchSingle (key, meta);

	return rc;
}

static int canonicalListSingle (Key * key, const Key * meta, int (*cmpFun) (const char *, const char *))
{
	const char * value = keyString (key);
	char ** list = stringToArray (keyString (meta), ",");
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


static int canonicalListArray (Key * key, const Key * meta, int (*cmpFun) (const char *, const char *))
{
	KeySet * lists = elektraMetaArrayToKS (key, keyName (meta));
	Key * elem = NULL;
	int rc = 0;
	while ((elem = ksNext (lists)) != NULL)
	{
		rc = canonicalListSingle (key, elem, cmpFun);
		if (rc != 0) break;
	}
	ksDel (lists);
	return rc;
}

static int canonicalCaseSensitiveList (Key * key, const Key * meta)
{
	int rc = 0;
	if (keyString (meta)[0] == '#')
		rc = canonicalListArray (key, meta, &strcmp);
	else
		rc = canonicalListSingle (key, meta, &strcmp);
	return rc;
}

static int canonicalCaseInsensitiveList (Key * key, const Key * meta)
{
	int rc = 0;
	if (keyString (meta)[0] == '#')
		rc = canonicalListArray (key, meta, &strcasecmp);
	else
		rc = canonicalListSingle (key, meta, &strcasecmp);
	return rc;
}

int elektraCanonicalGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/canonical"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/canonical", KEY_VALUE, "canonical plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/canonical/exports", KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/open", KEY_FUNC, elektraCanonicalOpen, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/close", KEY_FUNC, elektraCanonicalClose, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/get", KEY_FUNC, elektraCanonicalGet, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/set", KEY_FUNC, elektraCanonicalSet, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/error", KEY_FUNC, elektraCanonicalError, KEY_END),
			keyNew ("system/elektra/modules/canonical/exports/checkconf", KEY_FUNC, elektraCanonicalCheckConfig, KEY_END),
#include ELEKTRA_README (canonical)
			keyNew ("system/elektra/modules/canonical/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	Key * cur = NULL;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * pattern = NULL;
		if (((pattern = keyGetMeta (cur, "transform/canonical/regex")) != NULL) &&
		    (keyGetMeta (cur, "transform/canonical/origvalue") == NULL))
		{
			canonicalRegex (cur, pattern);
		}
		if (((pattern = keyGetMeta (cur, "transform/canonical/fnmatch")) != NULL) &&
		    (keyGetMeta (cur, "transform/canonical/origvalue") == NULL))
		{
			canonicalFNMatch (cur, pattern);
		}
		if (((pattern = keyGetMeta (cur, "transform/canonical/list/insensitive")) != NULL) &&
		    (keyGetMeta (cur, "transform/canonical/origvalue") == NULL))
		{
			canonicalCaseInsensitiveList (cur, pattern);
		}
		if (((pattern = keyGetMeta (cur, "transform/canonical/list/sensitive")) != NULL) &&
		    (keyGetMeta (cur, "transform/canonical/origvalue") == NULL))
		{
			canonicalCaseSensitiveList (cur, pattern);
		}
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
		const Key * origKey = keyGetMeta (cur, "transform/canonical/origvalue");
		if (origKey)
		{
			keySetString (cur, keyString (origKey));
			keySetMeta (cur, keyName (origKey), 0);
		}
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraCanonicalError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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
            ELEKTRA_PLUGIN_OPEN,	&elektraCanonicalOpen,
            ELEKTRA_PLUGIN_CLOSE,	&elektraCanonicalClose,
            ELEKTRA_PLUGIN_GET,	&elektraCanonicalGet,
            ELEKTRA_PLUGIN_SET,	&elektraCanonicalSet,
            ELEKTRA_PLUGIN_ERROR,	&elektraCanonicalError,
            ELEKTRA_PLUGIN_END);
}

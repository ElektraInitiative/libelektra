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
#include <kdbhelper.h>
#include <kdbmeta.h>
#include <kdbutility.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>


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

static int canonicalRegexSingle (Key * key, const Key * meta)
{
	const char * value = keyString (key);
	const char * regexString = keyString (meta);
	fprintf (stderr, " === RegexSingle ===\n === %s - %s:(%s)\n", regexString, keyName (key), value);

	regex_t regex;
	int ret = regcomp (&regex, regexString, REG_EXTENDED);

	if (ret != 0)
	{
		char buffer[1000];
		regerror (ret, &regex, buffer, 999);
		fprintf (stderr, "regex error: %s\n", buffer);
		regfree (&regex);
		return -1;
	}
	int nomatch = 0;
	nomatch = regexec (&regex, value, 0, NULL, 0);
	regfree (&regex);
	if (!nomatch)
	{
		fprintf (stderr, "   %s matched %s\n", regexString, value);
		keySetMeta (key, "transform/canonical/origvalue", value);
		Key * searchKey = keyNew (keyName (meta), KEY_META_NAME, KEY_END);
		keyAddBaseName (searchKey, "canonical");
		fprintf (stderr, "searchKey: %s\n", keyName (searchKey));
		const char * canonicalValue = keyString (keyGetMeta (key, keyName (searchKey)));
		keyDel (searchKey);
		fprintf (stderr, "canonicalValueKey: %s\n", canonicalValue);
		keySetString (key, canonicalValue);
		return 1;
	}
	return 0;
}

static int canonicalRegexArray (Key * key, const Key * meta)
{
	KeySet * lists = elektraMetaArrayToKS (key, keyName (meta));
	Key * elem = NULL;
	const char * value = keyString (key);
	fprintf (stderr, " === RegexArray ===\n === %s:(%s)\n", keyName (key), value);
	int rc = 0;
	while ((elem = ksNext (lists)) != NULL)
	{
		fprintf (stderr, "elem: %s:(%s)\n", keyName (elem), keyString (elem));
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
	fprintf (stderr, " === FNMatchSingle ===\n === %s - %s:(%s)\n", pattern, keyName (key), value);
	if (!fnmatch (pattern, value, 0))
	{
		keySetMeta (key, "transform/canonical/origvalue", value);
		Key * searchKey = keyNew (keyName (meta), KEY_META_NAME, KEY_END);
		keyAddBaseName (searchKey, "canonical");
		fprintf (stderr, "searchKey: %s\n", keyName (searchKey));
		const char * canonicalValue = keyString (keyGetMeta (key, keyName (searchKey)));
		keyDel (searchKey);
		fprintf (stderr, "canonicalValueKey: %s\n", canonicalValue);
		keySetString (key, canonicalValue);
		return 1;
	}
	return 0;
}

static int canonicalFNMatchArray (Key * key, const Key * meta)
{
	KeySet * lists = elektraMetaArrayToKS (key, keyName (meta));
	Key * elem = NULL;
	const char * value = keyString (key);
	fprintf (stderr, " === FNMatchArray ===\n === %s:(%s)\n", keyName (key), value);
	int rc = 0;
	while ((elem = ksNext (lists)) != NULL)
	{
		fprintf (stderr, "elem: %s:(%s)\n", keyName (elem), keyString (elem));
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
	fprintf (stderr, " === ListSingle ===\n === %s:(%s)\n", keyName (key), value);
	char ** list = stringToArray (keyString (meta), ",");
	if (!list) return -1;
	int rc = 0;
	for (size_t i = 0; list[i] != NULL; ++i)
	{
		fprintf (stderr, "list[%zd] \'%s\'\n", i, list[i]);
		if (!cmpFun (value, list[i]))
		{
			keySetMeta (key, "transform/canonical/origvalue", value);
			Key * searchKey = keyNew (keyName (meta), KEY_META_NAME, KEY_END);
			keyAddBaseName (searchKey, "canonical");
			fprintf (stderr, "searchKey: %s\n", keyName (searchKey));
			const char * canonicalValue = keyString (keyGetMeta (key, keyName (searchKey)));
			keyDel (searchKey);
			fprintf (stderr, "canonicalValueKey: %s\n", canonicalValue);
			keySetString (key, canonicalValue);
			rc = 1;
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
	const char * value = keyString (key);
	fprintf (stderr, " === ListArray ===\n === %s:(%s)\n", keyName (key), value);
	int rc = 0;
	while ((elem = ksNext (lists)) != NULL)
	{
		fprintf (stderr, "elem: %s:(%s)\n", keyName (elem), keyString (elem));
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

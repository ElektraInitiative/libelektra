/**
 * @file
 *
 * @brief Source for mini plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "mini.h"

#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbutility.h>
#include <stdio.h>

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

// ===========
// = Private =
// ===========

static inline KeySet * elektraMiniContract ()
{
	return ksNew (30, keyNew ("system/elektra/modules/mini", KEY_VALUE, "mini plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/mini/exports", KEY_END),
		      keyNew ("system/elektra/modules/mini/exports/get", KEY_FUNC, elektraMiniGet, KEY_END),
		      keyNew ("system/elektra/modules/mini/exports/set", KEY_FUNC, elektraMiniSet, KEY_END),
#include ELEKTRA_README (mini)
		      keyNew ("system/elektra/modules/mini/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

static inline char * stripComment (char * line)
{
	char * current = line;
	char * before = NULL;

	/* As long as we are not the end of the string and
	   the current character is either not a comment marker or the comment marker was escaped */
	while (*current != '\0' && ((*current != '#' && *current != ';') || (before && *before == '\\')))
	{
		before = current;
		current++;
	}
	*current = '\0';
	return line;
}

static inline char * findUnescapedEquals (char * text)
{
	char * equals = text;
	char * before = NULL;

	while (*equals != '\0' && (*equals != '=' || (before && *before == '\\')))
	{
		before = equals++;
	}
	return equals;
}

static inline void parseLine (char * line, size_t lineNumber, KeySet * keySet, Key * parentKey)
{
	char * pair = elektraStrip (stripComment (line));

	if (*pair == '\0')
	{
		return;
	}

	char * equals = findUnescapedEquals (pair);
	if (!equals)
	{
		fprintf (stderr, "Ignored line %lu since “%s” does not contain a valid key value pair\n", lineNumber, pair);
		return;
	}

	*equals = '\0';

	char * name = elektraStrip (pair);
	char * value = elektraStrip (equals + 1);

	Key * key = keyNew (keyName (parentKey), KEY_END);
	keyAddName (key, name);
	keySetString (key, value);
	ELEKTRA_LOG_DEBUG ("Name:  “%s”", keyName (key));
	ELEKTRA_LOG_DEBUG ("Value: “%s”", keyString (key));

	ksAppendKey (keySet, key);
}

static int parseINI (FILE * file, KeySet * keySet, Key * parentKey)
{
	char * line = NULL;
	size_t capacity = 0;
	int errorNumber = errno;

	for (size_t lineNumber = 1; getline (&line, &capacity, file) != -1; ++lineNumber)
	{
		ELEKTRA_LOG_DEBUG ("Read Line %lu: %s", lineNumber, line);
		parseLine (line, lineNumber, keySet, parentKey);
	}

	elektraFree (line);

	if (!feof (file))
	{
		ELEKTRA_LOG_WARNING ("Did not reach end of configuration file “%s”", keyString (parentKey));
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_NOEOF, parentKey, strerror (errno));
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int parseFile (KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	ELEKTRA_LOG ("Read configuration data");
	int errorNumber = errno;
	FILE * source = fopen (keyString (parentKey), "r");

	if (!source)
	{
		ELEKTRA_LOG_WARNING ("Could not open file “%s” for reading: %s", keyString (parentKey), strerror (errno));
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int status = parseINI (source, returned, parentKey);

	fclose (source);
	return status;
}

// ====================
// = Plugin Interface =
// ====================

int elektraMiniGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/mini"))
	{
		ELEKTRA_LOG_DEBUG ("Retrieve plugin contract");
		KeySet * contract = elektraMiniContract ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return parseFile (returned, parentKey);
}

int elektraMiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	ELEKTRA_LOG ("Write configuration data");
	int errorNumber = errno;
	FILE * destination = fopen (keyString (parentKey), "w");

	if (!destination)
	{
		ELEKTRA_LOG_WARNING ("Could not open file “%s” for writing: %s", keyString (parentKey), strerror (errno));
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key * key;
	ksRewind (returned);
	while ((key = ksNext (returned)) != 0)
	{
		const char * name = elektraKeyGetRelativeName (key, parentKey);
		ELEKTRA_LOG_DEBUG ("Write mapping “%s=%s”", name, keyString (key));
		fprintf (destination, "%s=%s\n", name, keyString (key));
	}

	fclose (destination);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (mini)
{
	// clang-format off
	return elektraPluginExport ("mini",
		ELEKTRA_PLUGIN_GET,	&elektraMiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraMiniSet,
		ELEKTRA_PLUGIN_END);
}

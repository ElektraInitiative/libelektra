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
#include "values.h"

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

static inline void parseLine (char * line, KeySet * keySet, Key * parentKey)
{
	char * equals = strchr (line, '=');
	*equals = '\0';

	char * name = elektraStrip (line);
	char * value = elektraStrip (equals + 1);

	Key * key = keyNew (keyName (parentKey), KEY_END);
	keyAddName (key, name);
	keySetString (key, value);
	ELEKTRA_LOG_DEBUG ("Name:  “%s”\n", keyName (key));
	ELEKTRA_LOG_DEBUG ("Value: “%s”\n", keyString (key));

	ksAppendKey (keySet, key);
}

static int parseINI (FILE * file, KeySet * keySet, Key * parentKey)
{
	char * line = NULL;
	size_t length = 0;
	int errorNumber = errno;

	for (size_t lineNumber = 1; getline (&line, &length, file) != -1; ++lineNumber)
	{
		ELEKTRA_LOG_DEBUG ("Read Line %lu: %s", lineNumber, line);
		parseLine (line, keySet, parentKey);
	}

	elektraFree (line);

	if (!feof (file))
	{
		ELEKTRA_LOG_DEBUG ("Did not reach end of configuration file “%s”!\n", keyString (parentKey));
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_NOEOF, parentKey, strerror (errno));
		errno = errorNumber;
		return ERROR;
	}

	return KEYSET_MODIFIED;
}

static int parseFile (KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	// Retrieve values stored in file specified via `parentKey`
	int errorNumber = errno;
	FILE * source = fopen (keyString (parentKey), "r");

	// Could not open file for reading
	if (!source)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errorNumber;
		return ERROR;
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
	// Retrieve plugin contract
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/mini"))
	{
		KeySet * contract = elektraMiniContract ();
		ksAppend (returned, contract);
		ksDel (contract);

		return KEYSET_MODIFIED;
	}

	return parseFile (returned, parentKey);
}

int elektraMiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return KEYSET_UNCHANGED;
}


Plugin * ELEKTRA_PLUGIN_EXPORT (mini)
{
	// clang-format off
	return elektraPluginExport ("mini",
		ELEKTRA_PLUGIN_GET,	&elektraMiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraMiniSet,
		ELEKTRA_PLUGIN_END);
}

/**
 * @file
 *
 * @brief Source for yaml plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "yaml.h"

#include <stdio.h>

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

// ===========
// = Private =
// ===========

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
static inline KeySet * contractYaml ()
{
	return ksNew (30, keyNew ("system/elektra/modules/yaml", KEY_VALUE, "yaml plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports", KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports/get", KEY_FUNC, elektraYamlGet, KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports/set", KEY_FUNC, elektraYamlSet, KEY_END),
#include ELEKTRA_README (yaml)
		      keyNew ("system/elektra/modules/yaml/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

/**
 * @brief Close a given file handle opened for reading
 *
 * @pre The parameters `file`, and `parentKey` must not be `NULL`.
 *
 * @param file The file handle this function should close
 * @param errorNumber A saved value for `errno` this function should restore if it was unable to close `file`
 * @param parentKey A key that is used by this function to store error information if the function was unable to close `file`
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if `file` was closed successfully
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to close `file`
 */
static inline int closeFileRead (FILE * file, int errorNumber, Key * parentKey)
{
	ELEKTRA_ASSERT (file, "The Parameter `file` contains `NULL` instead of a valid file handle.");
	ELEKTRA_ASSERT (parentKey, "The Parameter `parentKey` contains `NULL` instead of a valid key.");

	if (fclose (file) != 0)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errorNumber;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/**
 * @brief Parse a file containing data specified in a very basic subset of YAML and store the obtained data in a given key set.
 *
 * @pre The parameters `returned`, and `parentKey` must not be `NULL`.
 *
 * @param returned A key set used to store the data contained in the file specified by the value of `parentKey`
 * @param parentKey The value of this key value pair stores the path to the file this function should parse
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the whole parsing process was successful
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if at least one part of the parsing process failed
 */
static int parseFile (KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	ELEKTRA_ASSERT (returned, "The Parameter `returned` contains `NULL` instead of a valid key set.");
	ELEKTRA_ASSERT (parentKey, "The Parameter `parentKey` contains `NULL` instead of a valid key.");

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

	return closeFileRead (source, errorNumber, parentKey);
}

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraYamlGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/yaml"))
	{
		ELEKTRA_LOG_DEBUG ("Retrieve plugin contract");
		KeySet * contract = contractYaml ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return parseFile (returned, parentKey);
}

/** @see elektraDocSet */
int elektraYamlSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yaml)
{
	return elektraPluginExport ("yaml", ELEKTRA_PLUGIN_GET, &elektraYamlGet, ELEKTRA_PLUGIN_SET, &elektraYamlSet, ELEKTRA_PLUGIN_END);
}

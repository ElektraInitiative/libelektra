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

#ifdef HAVE_LOGGER
#include <libgen.h>
#include <sys/param.h>
#endif

/* -- Data Structures ------------------------------------------------------------------------------------------------------------------- */

/** This enum specifies the possible states of the recursive descent parser. */
typedef enum {
	/** Unable to open file */
	ERROR_FILE_OPEN,
	/** Unable to close file */
	ERROR_FILE_CLOSE,
	/** Everything is okay */
	OK
} statusType;

/** This structure saves various data for the recursive descent parser used in this plugin. */
typedef struct
{
	/** The current state of the parsing engine */
	statusType status;

	/** The handle of the opened file */
	FILE * file;
	/** Current line inside `file` */
	size_t line;
	/** Current column inside `line` */
	size_t column;

	/** Saves filename and allows us to emit error information */
	Key * parentKey;
	/** Contains key values pairs we parsed (get direction) or data we need to write back (set direction) */
	KeySet * keySet;

	/** Stores previous value of `errno` */
	int errorNumber;
} parsingData;

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

// ===========
// = Private =
// ===========

/**
 * @brief Open a file for reading
 *
 * @pre The parameters `data` and `data->parentKey` must not be `NULL`
 *
 * @param data Saves the filename of the file this function opens
 *
 * @retval The updated parsing structure. If there were any errors opening the file, then this function sets the type of the parsing
 * 	   structure to `ERROR_FILE_OPEN`.
 */
static parsingData * openFile (parsingData * const data)
{
	ELEKTRA_ASSERT (data, "The Parameter `data` contains `NULL`.");
	ELEKTRA_ASSERT (data->parentKey, "The Parameter `parentKey` contains `NULL`.");

	data->file = fopen (keyString (data->parentKey), "r");

	if (!data->file)
	{
		ELEKTRA_LOG_WARNING ("Could not open file “%s” for reading: %s", keyString (data->parentKey), strerror (errno));
		ELEKTRA_SET_ERROR_GET (data->parentKey);
		errno = data->errorNumber;
		data->status = ERROR_FILE_OPEN;
	}
	return data;
}

/**
 * @brief Close a file handle opened for reading
 *
 * @pre The parameters `data` must not be `NULL`
 *
 * @param data Saves the file handle this function closes
 *
 * @retval The updated parsing structure. If there were any errors closing the file, then this function sets the type of the parsing
 * 	   structure to `ERROR_FILE_CLOSE`.
 */
static parsingData * closeFileRead (parsingData * const data)
{
	ELEKTRA_ASSERT (data, "The Parameter `data` contains `NULL`.");

	if (data->file && fclose (data->file) != 0)
	{
		ELEKTRA_SET_ERROR_GET (data->parentKey);
		errno = data->errorNumber;
		data->status = ERROR_FILE_CLOSE;
	}
	return data;
}

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
static KeySet * contractYaml ()
{
	return ksNew (30, keyNew ("system/elektra/modules/yaml", KEY_VALUE, "yaml plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports", KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports/get", KEY_FUNC, elektraYamlGet, KEY_END),
		      keyNew ("system/elektra/modules/yaml/exports/set", KEY_FUNC, elektraYamlSet, KEY_END),
#include ELEKTRA_README (yaml)
		      keyNew ("system/elektra/modules/yaml/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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

	parsingData * data = &(parsingData){
		.status = OK, .line = 1, .column = 1, .file = NULL, .parentKey = parentKey, .keySet = returned, .errorNumber = errno
	};

	openFile (data);
	return closeFileRead (data)->status == OK ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
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

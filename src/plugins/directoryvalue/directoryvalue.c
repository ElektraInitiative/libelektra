/**
 * @file
 *
 * @brief Source for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "directoryvalue.h"

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of the plugin.
 */
static KeySet * directoryValueContract (void)
{
	return ksNew (30,
		      keyNew ("system/elektra/modules/directoryvalue", KEY_VALUE, "directoryvalue plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/directoryvalue/exports", KEY_END),
		      keyNew ("system/elektra/modules/directoryvalue/exports/get", KEY_FUNC, elektraDirectoryvalueGet, KEY_END),
		      keyNew ("system/elektra/modules/directoryvalue/exports/set", KEY_FUNC, elektraDirectoryvalueSet, KEY_END),
#include ELEKTRA_README (directoryvalue)
		      keyNew ("system/elektra/modules/directoryvalue/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

/**
 * @brief Convert `key` to a leaf value and copy it to `output`.
 *
 * - If `key` is already a leaf value it will not be changed and directly copied to `output`.
 * - If `key` is a directory value the function creates a new leaf value storing the data of the old directory key. The function also
 *   creates another empty node with the name of the directory value in this case. Both of these key will be copied to `output`.
 *
 * @pre The parameters `output`, `next` and `parent` must not be `NULL`.
 *
 * @param output This parameter stores the values this function creates.
 * @param key This parameter stores the key this function operates on.
 * @param next This parameter stores the key directly after `key`.
 * @param error The function uses this key to emit error information.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if everything went fine and the function copied `key` without any modifications to `output`.
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if everything went fine and the function converted `key` to a leaf value.
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to convert `key`.
 */
static int addDirectoryData (KeySet * output, Key const * const key, Key const * const next, Key * const error)
{
	ELEKTRA_NOT_NULL (next);
	ELEKTRA_NOT_NULL (error);
	ELEKTRA_NOT_NULL (output);

	if (!key)
	{
		ELEKTRA_LOG_DEBUG ("Key `key` is null");
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
	if (keyBaseName (next)[0] == '#' || keyIsBelow (key, next) != 1 || elektraStrCmp (keyName (key), keyName (error)) == 0)
	{
		ELEKTRA_LOG_DEBUG ("Key %s is an array or leaf value", keyName (key));
		ksAppendKey (output, keyDup (key));

		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	Key * directoryKey = keyNew (keyName (key), KEY_END);
	Key * dataKey = keyDup (key);
	if (keyAddBaseName (dataKey, DIRECTORY_POSTFIX) < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_DIRECTORY_VALUE_APPEND, error, "Could not append directory postfix to “%s”",
				    keyName (dataKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	ksAppendKey (output, directoryKey);
	ksAppendKey (output, dataKey);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief Convert the directory value saved in `key` back to a directory key.
 *
 * @param output The function uses this parameter to store the converted key.
 * @param convertedDirectory The function also stores the converted key in this key set.
 * @param key This parameter stores the current key this function operates on.
 * @param error The function uses this key to emit error information.
 *
 * @pre The parameters `output`, `convertedDirectory`, `key` and `error` must not be `NULL`.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if everything went fine
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to convert `key`
**/
static int convertToDirectory (KeySet * output, KeySet * convertedDirectory, Key * key, Key * error)
{
	ELEKTRA_NOT_NULL (output);
	ELEKTRA_NOT_NULL (convertedDirectory);
	ELEKTRA_NOT_NULL (key);
	ELEKTRA_NOT_NULL (error);

	size_t directoryKeyLength = elektraStrLen (keyName (key)) - DIRECTORY_POSTFIX_LENGTH - 1;

	int errorNumber = errno;
	char * directoryName = strndup (keyName (key), directoryKeyLength);
	if (!directoryName)
	{
		errno = errorNumber;
		ELEKTRA_MALLOC_ERROR (error, directoryKeyLength);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key * directoryKey = keyDup (key);
	keySetName (directoryKey, directoryName);
	elektraFree (directoryName);
	ksAppendKey (convertedDirectory, directoryKey);
	ksAppendKey (output, directoryKey);
	keyDel (key);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief Check `key` for a directory value, modify it and copy it to output.
 *
 * - If key contains a directory value (marked by `DIRECTORY_POSTFIX`) it will be converted to a directory key. After that the function
 *   saves the key in `output` and `dirValue`.
 * - If `key` has the same name as the last converted directory value (stored in `convertedDirectory`), then the function deletes the key.
 * - If `key` is a non-special key (none of the two cases above apply), then the function just appends the unmodified `key` to `output`.
 *
 * @pre The parameters `output`, `key`, `convertedDirectory` and `error` must not be `NULL`.
 *
 * @param output The function uses this key set to store the result of the key conversion.
 * @param key This parameter stores the current key this function operates on.
 * @param convertedDirectory This key set either stores the last converted directory value, or nothing (if this function was already
 *                           invoked with the empty version of the directory key as parameter).
 * @param error The function uses this key to emit error information.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if everything went fine and the function copied `key` without any modifications to `output`
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if everything went fine and the function either converted a directory value or did not add `key`
 *                                       to output
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to convert `key`
 */
static int removeDirectoryData (KeySet * output, Key * key, KeySet * convertedDirectory, Key * error)
{
	const char * baseName = keyBaseName (key);
	size_t baseNameLength = keyGetBaseNameSize (key);
	size_t minLengthBase = (DIRECTORY_POSTFIX_LENGTH < baseNameLength ? DIRECTORY_POSTFIX_LENGTH : baseNameLength) - 1;

	if (strncmp (baseName, DIRECTORY_POSTFIX, minLengthBase) == 0 && baseNameLength == DIRECTORY_POSTFIX_LENGTH)
	{
		ELEKTRA_LOG_DEBUG ("Convert leaf “%s” back to directory key", keyName (key));
		return convertToDirectory (output, convertedDirectory, key, error);
	}

	ELEKTRA_ASSERT (ksGetSize (convertedDirectory) <= 1, "More than one recent directory key");

	Key * directory = ksPop (convertedDirectory);
	if (directory && elektraStrCmp (keyName (key), keyName (directory)) == 0)
	{
		ELEKTRA_LOG_DEBUG ("Found old directory key “%s”", keyName (key));
		keyDel (directory);
		keyDel (key);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	ELEKTRA_LOG_DEBUG ("Append non-special key “%s”", keyName (key));
	ksAppendKey (output, key);
	if (directory) ksAppendKey (convertedDirectory, directory);
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocGet */
int elektraDirectoryvalueGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parent)
{
	if (!elektraStrCmp (keyName (parent), "system/elektra/modules/directoryvalue"))
	{
		KeySet * contract = directoryValueContract ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * key;
	KeySet * convertedDirectory = ksNew (0, KS_END);
	KeySet * output = ksNew (0, KS_END);
	int status = 0;
	while (status >= 0 && (key = ksPop (returned)) != NULL)
	{
		status |= removeDirectoryData (output, key, convertedDirectory, parent);
	}
	ksCopy (returned, output);
	ksDel (convertedDirectory);
	ksDel (output);

	return status;
}

/** @see elektraDocSet */
int elektraDirectoryvalueSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parent)
{
	ksRewind (returned);
	Key * current = NULL;
	Key * next;
	int status = 0;
	KeySet * output = ksNew (0, KS_END);

	while (status >= 0 && (next = ksNext (returned)) != NULL)
	{
		status |= addDirectoryData (output, current, next, parent);
		current = next;
	}
	// Last key is always a leaf value
	if (current) ksAppendKey (output, current);

	ksCopy (returned, output);
	ksDel (output);
	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (directoryvalue)
{
	return elektraPluginExport ("directoryvalue", ELEKTRA_PLUGIN_GET, &elektraDirectoryvalueGet, ELEKTRA_PLUGIN_SET,
				    &elektraDirectoryvalueSet, ELEKTRA_PLUGIN_END);
}

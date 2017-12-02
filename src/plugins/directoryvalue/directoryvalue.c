/**
 * @file
 *
 * @brief Source for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include "directoryvalue.h"

#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <stdbool.h>

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

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
 * @brief Check if all keys directly below `key` are array entries.
 *
 * @param key This parameter contains the parent key of the array this function checks.
 * @param keys This parameter contains the key set `key` belongs to.
 *
 * @retval true if all keys below `key` are array entries
 * @retval false otherwise
 */
static bool onlyArrayEntriesDirectlyBelow (Key * key, KeySet * keys)
{
	KeySet * arrayEntries = elektraArrayGet (key, keys);
	KeySet * children = ksNew (ksGetSize (keys), KS_END);
	elektraKsFilter (children, keys, (int (*) (const Key *, void *)) & keyIsDirectBelow, (void *)key);

	bool onlyArrayEntries = ksGetSize (arrayEntries) == ksGetSize (children);

	ksDel (arrayEntries);
	ksDel (children);

	return onlyArrayEntries;
}

/**
 * @brief Split `input` into two key sets, one for array keys and one for all other keys.
 *
 * @pre The parameters `input`, `parents`, and `other` must not be `NULL`.
 *
 * @param input This parameter contains the key set this function splits.
 * @param parents The function stores all array parents in this key set.
 * @param other The function stores all non-array keys in this parameter.
 */
static void splitArray (KeySet * input, KeySet * parents, KeySet * other)
{
	ELEKTRA_NOT_NULL (input);
	ELEKTRA_NOT_NULL (parents);
	ELEKTRA_NOT_NULL (other);

	ksRewind (input);
	Key * key = ksNext (input);
	Key * next;

	while ((next = ksNext (input)) != NULL)
	{
		bool isArrayParent = keyIsBelow (key, next) && keyBaseName (next)[0] == '#' && onlyArrayEntriesDirectlyBelow (key, input);
		ksAppendKey (isArrayParent ? parents : other, keyDup (key));
		key = next;
	}
	// Last key can not be an array key
	if (key) ksAppendKey (other, key);
}

/**
 * @brief Split `input` into two key sets, one for directories (keys without children) and one for all other keys.
 *
 * @pre The parameters `input`, `directories` and `leaves` must not be `NULL`.
 *
 * @param input This parameter contains the key set this function splits.
 * @param directories The function stores all directories in this key set.
 * @param leaves The function stores all leaves (keys without children) in this parameter.
 */
static void splitDirectories (KeySet * input, KeySet * directories, KeySet * leaves)
{
	ELEKTRA_NOT_NULL (input);
	ELEKTRA_NOT_NULL (directories);
	ELEKTRA_NOT_NULL (leaves);

	ksRewind (input);
	Key * key = ksNext (input);
	Key * next;

	while ((next = ksNext (input)) != NULL)
	{
		ksAppendKey (keyIsBelow (key, next) ? directories : leaves, keyDup (key));
		key = next;
	}
	// Last key is always a leaf value
	if (key) ksAppendKey (leaves, key);
}

/**
 * @brief Convert all keys in `directories` to an empty key and a leaf key containing the data of the old key.
 *
 * @pre The parameters `directories` and `error` must not be `NULL`.
 *
 * @param directories This parameter contains a set of directory keys this function converts.
 * @param error The function uses this parameter to emit error information.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if everything went fine.
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to convert a key.
 */
static int convertDirectoriesToLeaves (KeySet * directories, Key * error)
{
	ELEKTRA_NOT_NULL (directories);
	ELEKTRA_NOT_NULL (error);

	KeySet * result = ksNew (0, KS_END);
	ksRewind (directories);
	Key * key;

	while ((key = ksNext (directories)) != NULL)
	{
		Key * dataKey = keyDup (key);
		// Only add extra leaf key for non-empty directory values
		if ((keyIsBinary (key) && keyGetValueSize (key) > 0) || keyGetValueSize (key) > 1)
		{
			Key * directoryKey = keyNew (keyName (key), KEY_END);
			if (keyAddBaseName (dataKey, DIRECTORY_POSTFIX) < 0)
			{
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_DIRECTORY_VALUE_APPEND, error,
						    "Could not append directory postfix to “%s”", keyName (dataKey));
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			ksAppendKey (result, directoryKey);
		}
		ksAppendKey (result, dataKey);
	}
	ksCopy (directories, result);
	ksDel (result);

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
	KeySet * leaves = ksNew (0, KS_END);
	KeySet * arrays = ksNew (0, KS_END);
	KeySet * directories = ksNew (0, KS_END);
	KeySet * withoutArrays = ksNew (0, KS_END);

	int status = ELEKTRA_PLUGIN_STATUS_SUCCESS;

	splitArray (returned, arrays, withoutArrays);
	splitDirectories (withoutArrays, directories, leaves);
	if (ksGetSize (directories) <= 0) status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	if (convertDirectoriesToLeaves (directories, parent) < 0) status = ELEKTRA_PLUGIN_STATUS_ERROR;
	ksCopy (returned, directories);
	ksAppend (returned, arrays);
	ksAppend (returned, leaves);

	ksDel (withoutArrays);
	ksDel (leaves);
	ksDel (directories);
	ksDel (arrays);

	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (directoryvalue)
{
	return elektraPluginExport ("directoryvalue", ELEKTRA_PLUGIN_GET, &elektraDirectoryvalueGet, ELEKTRA_PLUGIN_SET,
				    &elektraDirectoryvalueSet, ELEKTRA_PLUGIN_END);
}

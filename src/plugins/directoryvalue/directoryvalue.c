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
 * @pre The parameters `key`, and `keys` must not be `NULL`.
 *
 * @param key This parameter contains the parent key of the array this function checks.
 * @param keys This parameter contains the key set `key` belongs to.
 *
 * @retval true if all keys below `key` are array entries
 * @retval false otherwise
 */
static bool onlyArrayEntriesDirectlyBelow (Key * key, KeySet * keys)
{
	ELEKTRA_NOT_NULL (key);
	ELEKTRA_NOT_NULL (keys);

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
 * @pre The parameters `input`, `arrays`, and `other` must not be `NULL`.
 *
 * @param input This parameter contains the key set this function splits.
 * @param arrays The function stores all array parents in this key set.
 * @param other The function stores all non-array keys in this parameter.
 */
static void splitArray (KeySet * input, KeySet * arrays, KeySet * other)
{
	ELEKTRA_NOT_NULL (input);
	ELEKTRA_NOT_NULL (arrays);
	ELEKTRA_NOT_NULL (other);

	ksRewind (input);
	Key * key = ksNext (input);
	Key * next;

	while ((next = ksNext (input)) != NULL)
	{
		bool isArrayParent = keyIsBelow (key, next) && keyBaseName (next)[0] == '#' && onlyArrayEntriesDirectlyBelow (key, input);
		ksAppendKey (isArrayParent ? arrays : other, keyDup (key));
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
 * @brief Copy directory leaves (marked with `DIRECTORY_POSTFIX`) from `input` to leaves.
 *
 * @pre The parameters `input` and `leaves` must not be `NULL`.
 *
 * @param input The function searches for directory leaves in this key set.
 * @param leaves The function copies all directory leaves to this key set.
 */
static void splitDirectoryLeaves (KeySet * input, KeySet * const leaves)
{
	ELEKTRA_NOT_NULL (input);
	ELEKTRA_NOT_NULL (leaves);

	Key * key;
	KeySet * other = ksNew (0, KS_END);

	ksRewind (input);
	while ((key = ksNext (input)) != NULL)
	{
		size_t baseNameLength = keyGetBaseNameSize (key);
		size_t minLengthBase = (DIRECTORY_POSTFIX_LENGTH < baseNameLength ? DIRECTORY_POSTFIX_LENGTH : baseNameLength) - 1;
		bool isLeaf =
			strncmp (keyBaseName (key), DIRECTORY_POSTFIX, minLengthBase) == 0 && baseNameLength == DIRECTORY_POSTFIX_LENGTH;
		ksAppendKey (isLeaf ? leaves : other, keyDup (key));
	}
	ksCopy (input, other);
	ksDel (other);
}

/**
 * @brief Remove the directory prefix (`DIRECTORY_POSTFIX`) from all keys in `leaves`.
 *
 * @pre The parameters `leaves` and `error` must not be `NULL`.
 * @pre The name of all keys in `leaves` must end with `DIRECTORY_POSTFIX`.
 *
 * @param leaves This parameter contains a set of directory leaves this function converts.
 * @param error The function uses this parameter to emit error information.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if everything went fine.
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to remove the directory prefix.
 */
static int convertDirectoryLeaves (KeySet * leaves, Key * error)
{
	ELEKTRA_NOT_NULL (leaves);
	ELEKTRA_NOT_NULL (error);

	KeySet * result = ksNew (0, KS_END);
	ksRewind (leaves);

	Key * key;
	while ((key = ksNext (leaves)) != NULL)
	{
		size_t directoryKeyLength = elektraStrLen (keyName (key)) - DIRECTORY_POSTFIX_LENGTH - 1;

		int errorNumber = errno;
		char * directoryName = strndup (keyName (key), directoryKeyLength);
		if (!directoryName)
		{
			errno = errorNumber;
			ELEKTRA_MALLOC_ERROR (error, directoryKeyLength);
			ksDel (result);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		Key * directoryKey = keyDup (key);
		keySetName (directoryKey, directoryName);
		elektraFree (directoryName);
		ksAppendKey (result, directoryKey);
	}

	ksCopy (leaves, result);
	ksDel (result);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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

	KeySet * leaves = ksNew (0, KS_END);
	int status = ELEKTRA_PLUGIN_STATUS_SUCCESS;
	splitDirectoryLeaves (returned, leaves);
	if (ksGetSize (leaves) <= 0) status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	if (convertDirectoryLeaves (leaves, parent) < 0)
	{
		status = ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else
	{
		ksAppend (returned, leaves);
	}
	ksDel (leaves);
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

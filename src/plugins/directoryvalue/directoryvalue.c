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

// =======
// = Get =
// =======

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
 * @brief Split `input` into two key sets, one for array leaf keys (index 0, marked with `ARRAY_VALUE_PREFIX`) and one for all other keys.
 *
 * @pre The parameters `input` and `leaves` must not be `NULL`.
 *
 * @param input The function removes all array leaves from this key set.
 * @param leaves The function uses this key set to store all array leaves.
 */
static void splitArrayLeaves (KeySet * input, KeySet * const leaves)
{
	ELEKTRA_NOT_NULL (input);
	ELEKTRA_NOT_NULL (leaves);

	Key * key;
	KeySet * other = ksNew (0, KS_END);

	ksRewind (input);
	while ((key = ksNext (input)) != NULL)
	{
		bool isArrayLeaf = elektraStrCmp (keyBaseName (key), "#0") == 0 && keyIsString (key) && keyGetValueSize (key) > 1 &&
				   strncmp (keyString (key), ARRAY_VALUE_PREFIX, ARRAY_VALUE_PREFIX_LENGTH - 1) == 0;
		if (isArrayLeaf)
		{
			ELEKTRA_LOG_DEBUG ("Key “%s” is an array leaf", keyName (key));
		}
		ksAppendKey (isArrayLeaf ? leaves : other, keyDup (key));
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

/**
 * @brief Remove the index and directory value prefix (`ARRAY_VALUE_PREFIX`) from all keys in `leaves`.
 *
 * @pre The parameters `leaves` and `error` must not be `NULL`.
 * @pre The name of all keys in `leaves` must end with `#0`.
 * @pre The values in all keys of leaves must start with `ARRAY_VALUE_PREFIX`.
 *
 * @param leaves This parameter contains a set of array leaves this function converts.
 * @param error The function uses this parameter to emit error information.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if everything went fine.
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to remove the directory prefix.
 */
static int convertArrayLeaves (KeySet * leaves, Key * error)
{
	ELEKTRA_NOT_NULL (leaves);
	ELEKTRA_NOT_NULL (error);

	KeySet * result = ksNew (0, KS_END);
	ksRewind (leaves);

	Key * key;
	while ((key = ksNext (leaves)) != NULL)
	{
		// Convert array element to array parent
		size_t directoryKeyLength = elektraStrLen (keyName (key)) - sizeof ("#0");
		int errorNumber = errno;
		char * arrayName = strndup (keyName (key), directoryKeyLength);
		if (!arrayName)
		{
			errno = errorNumber;
			ELEKTRA_MALLOC_ERROR (error, directoryKeyLength);
			ksDel (result);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		Key * arrayKey = keyDup (key);
		keySetName (arrayKey, arrayName);
		elektraFree (arrayName);

		// Remove value prefix
		keySetString (arrayKey, keyString (key) + ARRAY_VALUE_PREFIX_LENGTH - 1);

		ELEKTRA_LOG_DEBUG ("Converted array key “%s” contains value “%s”", keyName (arrayKey), keyString (arrayKey));

		ksAppendKey (result, arrayKey);
	}
	ksCopy (leaves, result);
	ksDel (result);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief Update all arrays in `returned`.
 *
 * The function pops the first array element in `returned` for every array saved in `arrays`. It also decreases the index of the remaining
 * array elements, and stores the array parents from `arrays` in `returned`. The function also saves the last index of all modified arrays
 * in the metadata of the array parent.
 *
 * @pre The parameters `returned` and `arrays` must not be `NULL`.
 *
 * @param returned This parameter contains the array elements of the parents stored in `arrays`. The function updates this key set according
 *                 to the description above.
 * @param arrays This parameter saves the parent of every array in `returned` this function updates.
 */
static void updateArrays (KeySet * returned, KeySet * const arrays)
{
	KeySet * updatedArrays = ksNew (0, KS_END);
	KeySet * lastElements = ksNew (0, KS_END);
	ksRewind (arrays);

	Key * array;
	while ((array = ksNext (arrays)) != NULL)
	{
		Key * updatedArray = keyDup (array);
		KeySet * elements = elektraArrayGet (updatedArray, returned);

		ksRewind (elements);
		Key * element;
		bool last = true;
		while ((element = ksPop (elements)) != NULL)
		{
			Key * updatedElement = keyDup (element);
			elektraArrayDecName (updatedElement);
			ksAppendKey (updatedArrays, updatedElement);
			if (last)
			{
				ksAppendKey (lastElements, element);
				keySetMeta (updatedArray, "array", keyBaseName (updatedElement));
				ELEKTRA_LOG_DEBUG ("New Last index of array “%s” is “%s”", keyName (array), keyBaseName (updatedElement));
				ksAppendKey (updatedArrays, updatedArray);
				last = false;
			}
			keyDel (element);
		}
		ksDel (elements);
	}

	ksRewind (lastElements);
	Key * key;
	while ((key = ksNext (lastElements)) != NULL)
	{
		keyDel (ksLookupByName (returned, keyName (key), KDB_O_POP));
	}
	ksAppend (returned, updatedArrays);
	ksDel (lastElements);
	ksDel (updatedArrays);
}

// =======
// = Set =
// =======

/**
 * @brief Check if `key` is directly below `parent`.
 *
 * @pre The parameters `key` and `parent` must not be `NULL`.
 *
 * @param key This parameter contains a possible child of `parent`.
 * @param parent This parameter contains a possible parent of `key`.
 *
 * @retval true if `key` is directly below `parent`
 * @retval false otherwise
 */
inline static int isChild (const Key * key, void * parent)
{
	ELEKTRA_NOT_NULL (key);
	ELEKTRA_NOT_NULL (parent);

	return keyIsDirectBelow ((const Key *) parent, key);
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
	elektraKsFilter (children, keys, &isChild, (void *) key);

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
static void splitArrays (KeySet * const input, KeySet * arrays, KeySet * other)
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
 * @brief Increase the index of all keys below a given array parent by one.
 *
 * @pre The parameters `array`, `keys` and `errorKey` must not be `NULL`.
 *
 * @param array This parameter contains an array parent.
 * @param keys This key set contains the key set `parent` belongs to.
 * @param errorKey The function uses this parameter to emit error information.
 *
 * @returns The function returns a new key set containing modified versions of the children of `array`, if everything went fine. If there
 *          was an error, the function returns `NULL` instead.
 */
static KeySet * childrenIncreaseIndex (Key * const array, KeySet * const keys, Key * errorKey)
{
	ELEKTRA_NOT_NULL (array);
	ELEKTRA_NOT_NULL (keys);
	ELEKTRA_NOT_NULL (errorKey);

	KeySet * children = elektraArrayGet (array, keys);
	KeySet * childrenNewIndex = ksNew (0, KS_END);
	Key * child;
	bool error = false;

	while (!error && (child = ksPop (children)) != NULL)
	{
		Key * key = keyDup (child);
		keyDel (child);
		if (elektraArrayIncName (key) < 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_DIRECTORY_VALUE_ARRAY, errorKey, "Could not increase array index for key “%s”",
					    keyName (key));
			error = true;
		}
		ksAppendKey (childrenNewIndex, key);
	}
	ksDel (children);
	if (error)
	{
		ksDel (childrenNewIndex);
		return NULL;
	}

	return childrenNewIndex;
}

/**
 * @brief Convert an array parent to an array leaf with index 0.
 *
 * @pre The parameters `array` and `error` must not be `NULL`.
 *
 * @param array This parameter contains the array parent this function converts.
 * @param error The function uses this parameter to emit error information.
 *
 * @returns The function returns a modified version of `array` on success. If there was an error the function returns `NULL` instead.
 */
static Key * arrayKeyToLeaf (Key * array, Key * error)
{
	ELEKTRA_NOT_NULL (array);
	ELEKTRA_NOT_NULL (error);

	Key * leaf = keyDup (array);
	if (keyAddBaseName (leaf, "#0") < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_DIRECTORY_VALUE_ARRAY, error, "Could not append array index 0 to “%s”", keyName (leaf));
		return NULL;
	}
	size_t valueLength = keyGetValueSize (array);

	int errorNumber = errno;
	char * value = elektraMalloc (ARRAY_VALUE_PREFIX_LENGTH + valueLength);
	if (!value)
	{
		errno = errorNumber;
		ELEKTRA_MALLOC_ERROR (error, valueLength);
		return NULL;
	}
	strcpy (value, ARRAY_VALUE_PREFIX);						 //! OCLint (constant conditional operator)
	strncpy (value + ARRAY_VALUE_PREFIX_LENGTH - 1, keyString (array), valueLength); //! OCLint (constant conditional operator)
	keySetString (leaf, value);
	elektraFree (value);

	return leaf;
}

/**
 * @brief Convert array key sets containing directory information (values in parent) to array key sets without directory information.
 *
 * The function creates a new key set and stores the modified array parents and leaves in `arrays`. The function inserts a new array child
 * at index 0 for every non-empty array key stored in `arrays`.
 *
 * @pre The parameters `arrays`, `leaves` and `error` must not be `NULL`.
 *
 * @param arrays This parameter contains the array parents for which this function inserts new leaves.
 * @param leaves This key set stores all array elements of the parents stored in `arrays`.
 * @param error The function uses this parameter to emit error information.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if everything went fine
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the function was unable to convert a key
 */
static int convertArraysToLeaves (KeySet * arrays, KeySet * const leaves, Key * error)
{
	ELEKTRA_NOT_NULL (arrays);
	ELEKTRA_NOT_NULL (leaves);
	ELEKTRA_NOT_NULL (error);

	Key * parent;
	KeySet * result = ksNew (0, KS_END);

	ksRewind (arrays);
	while ((parent = ksNext (arrays)) != NULL)
	{
		// Ignore empty and binary keys
		if (keyIsBinary (parent) || keyGetValueSize (parent) <= 1)
		{
			ksAppendKey (result, keyDup (parent));
			continue;
		}
		// Increase child index
		KeySet * children = childrenIncreaseIndex (parent, leaves, error);
		if (!children) return ELEKTRA_PLUGIN_STATUS_ERROR;

		// Add new key at index 0 with value prefix `___dirdata: `
		Key * leaf = arrayKeyToLeaf (parent, error);
		if (!leaf) return ELEKTRA_PLUGIN_STATUS_ERROR;
		keySetMeta (leaf, "array", 0); // Delete (incorrect) information about last index
		ksAppendKey (result, leaf);
		ksAppendKey (result, keyNew (keyName (parent), KS_END));
		ksAppend (result, children);
		ksDel (children);
	}
	ksCopy (arrays, result);
	ksDel (result);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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
		Key * directoryKey = keyNew (keyName (key), KEY_END);
		if (keyAddBaseName (dataKey, DIRECTORY_POSTFIX) < 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_DIRECTORY_VALUE_APPEND, error, "Could not append directory postfix to “%s”",
					    keyName (dataKey));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		ksAppendKey (result, directoryKey);
		ksAppendKey (result, dataKey);
	}
	ksCopy (directories, result);
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

	KeySet * directoryLeaves = ksNew (0, KS_END);
	KeySet * arrayLeaves = ksNew (0, KS_END);

	int status = ELEKTRA_PLUGIN_STATUS_SUCCESS;

	splitDirectoryLeaves (returned, directoryLeaves);
	splitArrayLeaves (returned, arrayLeaves);

	if (ksGetSize (directoryLeaves) <= 0 && ksGetSize (arrayLeaves) <= 0) status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	if (convertDirectoryLeaves (directoryLeaves, parent) < 0) status = ELEKTRA_PLUGIN_STATUS_ERROR;
	if (convertArrayLeaves (arrayLeaves, parent) >= 0)
	{
		updateArrays (returned, arrayLeaves);
	}
	else
	{
		status = ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ksAppend (returned, directoryLeaves);

	ksDel (directoryLeaves);
	ksDel (arrayLeaves);
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

	splitArrays (returned, arrays, withoutArrays);
	splitDirectories (withoutArrays, directories, leaves);
	if (ksGetSize (directories) <= 0 && ksGetSize (arrays) <= 0) status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	if (convertDirectoriesToLeaves (directories, parent) < 0) status = ELEKTRA_PLUGIN_STATUS_ERROR;
	if (convertArraysToLeaves (arrays, withoutArrays, parent) < 0) status = ELEKTRA_PLUGIN_STATUS_ERROR;
	ksCopy (returned, directories);
	ksAppend (returned, leaves);
	ksAppend (returned, arrays); // Overwrite old array elements (stored in leaves) with new array elements

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

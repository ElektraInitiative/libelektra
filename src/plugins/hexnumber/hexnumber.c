/**
 * @file
 *
 * @brief Source for hexnumber plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "hexnumber.h"

#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct
{
	bool forceConversion;
	ElektraKeyset * integerTypes;
} HexnumberData;

/**
 * Creates a KeySet representing the contract of this plugin.
 */
static ElektraKeyset * elektraContract (void)
{
	return elektraKeysetNew (
		30,
		elektraKeyNew ("system:/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME, ELEKTRA_KEY_VALUE, "hexnumber plugin waits for your orders",
			ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/exports", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/exports/get", ELEKTRA_KEY_FUNC, elektraHexnumberGet, ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/exports/set", ELEKTRA_KEY_FUNC, elektraHexnumberSet, ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/exports/close", ELEKTRA_KEY_FUNC, elektraHexnumberClose,
			ELEKTRA_KEY_END),

#include ELEKTRA_README

		elektraKeyNew ("system:/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
}

/**
 * Converts a Key with a hexadecimal number value to a Key with a decimal value.
 *
 * @pre The key has to actually contain a hexadecimal number string.
 *
 * @param key The Key which should be converted.
 * @param parentKey The parent Key used to set errors.
 *
 * @retval #ELEKTRA_PLUGIN_STATUS_SUCCESS if the Key was successfully converted
 * @retval #ELEKTRA_PLUGIN_STATUS_ERROR if the Key could not be converted
 */
static int convertHexToDec (ElektraKey * key, ElektraKey * parentKey)
{
	// get hex string from key
	const char * hexValue = elektraKeyString (key);

	int errnoSaved = errno;

	// TODO: use supermacro from PR #1850 once merged
	// convert hex string to long long int
	errno = 0;
	char * endPtr;
	unsigned long long int value = strtoull (hexValue, &endPtr, 16);
	if (errno == ERANGE && value == ULLONG_MAX)
	{
		errno = errnoSaved;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Hexadecimal number %s out of range 0 to %llu", hexValue, ULLONG_MAX);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else if ((errno != 0 && value == 0) || endPtr == hexValue || *endPtr != '\0')
	{
		errno = errnoSaved;
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Hexadecimal number '%s' could not be read", hexValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	errno = errnoSaved;

	// convert long long int back to string (formatted as decimal)
	int result = snprintf (NULL, 0, "%llu", value);
	if (result < 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unable to convert '%s' into decimal", hexValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const size_t length = (size_t) result + 1;
	char * decValue = elektraMalloc (length);
	if (!decValue)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	result = snprintf (decValue, length, "%llu", value);
	if (result < 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unable to convert '%s' into decimal", hexValue);
		elektraFree (decValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// set decimal string in key and set internal metadata
	elektraKeySetString (key, decValue);
	elektraKeySetMeta (key, ELEKTRA_HEXNUMBER_META_KEY, "1");

	elektraFree (decValue);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * Converts a Key with a decimal number value to a Key with a hexadecimal value.
 *
 * @pre The key has to actually contain a decimal number string.
 *
 * @param key The Key whose value should be converted.
 * @param parentKey The parent Key used to set errors.
 *
 * @retval #ELEKTRA_PLUGIN_STATUS_SUCCESS if the Key was successfully converted
 * @retval #ELEKTRA_PLUGIN_STATUS_ERROR if the Key could not be converted
 */
static int convertDecToHex (ElektraKey * key, ElektraKey * parentKey)
{
	// get decimal string from key
	const char * decValue = elektraKeyString (key);

	int errnoSaved = errno;

	// convert hex string to long long int
	errno = 0;
	char * endPtr;
	unsigned long long int value = strtoull (decValue, &endPtr, 10);
	if (errno == ERANGE && value == ULLONG_MAX)
	{
		errno = errnoSaved;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Decimal number %s out of range 0 to %llu", decValue, ULLONG_MAX);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else if ((errno != 0 && value == 0) || endPtr == decValue)
	{
		errno = errnoSaved;
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Decimal number '%s' could not be read", decValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	errno = errnoSaved;

	// convert long long int back to string (formatted as hexadecimal)
	const int result = snprintf (NULL, 0, "0x%llx", value);
	if (result < 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unable to convert '%s' into hexadecimal", decValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const size_t length = (size_t) result + 1;
	char * hexValue = elektraMalloc (length);
	if (!hexValue)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (snprintf (hexValue, length, "0x%llx", value) < 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unable to convert '%s' into hexadecimal", decValue);
		elektraFree (hexValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// set hex string in key and unset internal metadata
	elektraKeySetString (key, hexValue);
	elektraKeySetMeta (key, ELEKTRA_HEXNUMBER_META_KEY, "0");


	elektraFree (hexValue);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * Checks whether a given Key's value is a hexadecimal string.
 *
 * @param key The Key that should be checked.
 *
 * @retval #true if the Key's value starts with 0x
 * @retval #false otherwise
 */
static bool isHexString (const ElektraKey * key)
{
	return elektraStrNCaseCmp (elektraKeyString (key), "0x", 2) == 0;
}

/**
 * Checks whether a given Key is specified to have a hexadecimal base.
 *
 * @param key The Key that should be checked.
 *
 * @retval #true if the Key's metadata contains the key /unit/base and its value is hex
 * @retval #false otherwise
 */
static bool isHexUnitBase (const ElektraKey * key)
{
	const ElektraKey * unitBaseMeta = elektraKeyGetMeta (key, "unit/base");
	return elektraStrCmp (elektraKeyString (unitBaseMeta), "hex") == 0;
}

/**
 * Filter function used in hasType() for call to elektraKsFilter()
 */
static int __hasTypeFilter (const ElektraKey * key, void * argument)
{
	const char * type = (const char *) argument;
	return elektraKeyIsString (key) && strcmp (type, elektraKeyString (key)) == 0;
}

/**
 * Checks whether a given key has one of the given types.
 *
 * @param key The Key that should be checked.
 * @param types An array of strings containing possible values for the /type metadata.
 * @param typeCount The number of strings in the @p types array.
 *
 * @retval #true if the Key's type metadata is contained in the types list
 * @retval #false otherwise
 */
static bool hasType (const ElektraKey * key, ElektraKeyset * types)
{
	const ElektraKey * typeMeta = elektraKeyGetMeta (key, "type");
	if (!typeMeta || !types)
	{
		return false;
	}

	const char * type = elektraKeyString (typeMeta);
	ElektraKeyset * res = elektraKeysetNew ((size_t) elektraKeysetGetSize (types), ELEKTRA_KS_END);
	elektraKsFilter (res, types, &__hasTypeFilter, (void *) type);

	const ssize_t size = elektraKeysetGetSize (res);
	elektraKeysetDel (res);

	return size > 0;
}

/**
 * Checks whether a given Key's type metadata is #ELEKTRA_HEXNUMBER_META_TYPE.
 *
 * @param key The Key that should be checked.
 *
 * @retval #true if the Key's type metadata is #ELEKTRA_HEXNUMBER_META_TYPE
 * @retval #false otherwise
 */
static bool hasHexType (const ElektraKey * key)
{
	const ElektraKey * typeMeta = elektraKeyGetMeta (key, ELEKTRA_HEXNUMBER_META_KEY);
	return typeMeta && elektraStrCmp (elektraKeyString (typeMeta), "1") == 0;
}

/**
 * Parse the configuration as described in README.md
 * @param config the configuration KeySet provided by elektraPluginGetConfig
 * @param data pointer to the struct to store the configuration in
 * @param errorKey Key used to store error information
 */
int parseConfig (ElektraKeyset * config, HexnumberData * data, ElektraKey * errorKey)
{
	ElektraKey * forceKey = elektraKeysetLookupByName (config, "/force", 0);
	if (forceKey)
	{
		data->forceConversion = true;
	}

	ElektraKey * typesKey = elektraKeyNew ("/accept/type", ELEKTRA_KEY_END);
	ElektraKeyset * types = elektraArrayGet (typesKey, config);
	elektraKeyDel (typesKey);

	if (!types)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "Could not parse config. Types not set correctly");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	data->integerTypes = types;

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * Establish the plugin contract and convert all hexadecimal values in the KeySet to decimal.
 *
 * @param handle This parameter stores the configuration of the plugin.
 * @param returned This parameter specifies the key set that this function updates.
 * @param parentKey The function stores information about errors/warnings in this parameter.
 *
 * @retval #ELEKTRA_PLUGIN_STATUS_SUCCESS if any keys were updated
 * @retval #ELEKTRA_PLUGIN_STATUS_NO_UPDATE if \p returned was not modified
 * @retval #ELEKTRA_PLUGIN_STATUS_ERROR on failure
 */
int elektraHexnumberGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME))
	{
		ElektraKeyset * contract = elektraContract ();
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	HexnumberData * data = elektraPluginGetData (handle);
	if (!data)
	{
		ElektraKeyset * config = elektraPluginGetConfig (handle);
		data = elektraCalloc (sizeof (HexnumberData));
		if (parseConfig (config, data, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			elektraFree (data);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		elektraPluginSetData (handle, data);
	}

	ElektraKey * cur;
	elektraKeysetRewind (returned);

	ElektraKeyset * defaultIntegerTypes = elektraKeysetNew (7, elektraKeyNew ("system:/accept/type/#0", ELEKTRA_KEY_VALUE, "byte", ELEKTRA_KEY_END),
					      elektraKeyNew ("system:/accept/type/#1", ELEKTRA_KEY_VALUE, "short", ELEKTRA_KEY_END),
					      elektraKeyNew ("system:/accept/type/#2", ELEKTRA_KEY_VALUE, "long", ELEKTRA_KEY_END),
					      elektraKeyNew ("system:/accept/type/#3", ELEKTRA_KEY_VALUE, "long_long", ELEKTRA_KEY_END),
					      elektraKeyNew ("system:/accept/type/#4", ELEKTRA_KEY_VALUE, "unsigned_short", ELEKTRA_KEY_END),
					      elektraKeyNew ("system:/accept/type/#5", ELEKTRA_KEY_VALUE, "unsigned_long", ELEKTRA_KEY_END),
					      elektraKeyNew ("system:/accept/type/#6", ELEKTRA_KEY_VALUE, "unsigned_long_long", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		if (!elektraKeyIsString (cur))
		{
			continue;
		}

		bool hexString = isHexString (cur);
		if (isHexUnitBase (cur))
		{
			if (hexString)
			{
				status |= convertHexToDec (cur, parentKey);
			}
			else
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
					parentKey, "Key '%s' has unit/base metadata set as hex but value '%s' does not start with 0x",
					elektraKeyName (cur), elektraKeyString (cur));
				status |= ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}
		else if (hexString && (data->forceConversion || hasType (cur, data->integerTypes) || hasType (cur, defaultIntegerTypes)))
		{
			status |= convertHexToDec (cur, parentKey);
		}
	}

	elektraKeysetDel (defaultIntegerTypes);

	return status;
}

/**
 * Convert all values in the KeySet originally stored as hexadecimal (marked by type metdata) to hexadecimal.
 *
 * @param handle This parameter stores the configuration of the plugin.
 * @param returned This parameter specifies the key set that this function updates.
 * @param parentKey The function stores information about errors/warnings in this parameter.
 *
 * @retval #ELEKTRA_PLUGIN_STATUS_SUCCESS if any keys were updated
 * @retval #ELEKTRA_PLUGIN_STATUS_NO_UPDATE if \p returned was not modified
 * @retval #ELEKTRA_PLUGIN_STATUS_ERROR on failure
 */
int elektraHexnumberSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	HexnumberData * data = elektraPluginGetData (handle);
	if (!data)
	{
		ElektraKeyset * config = elektraPluginGetConfig (handle);
		data = elektraCalloc (sizeof (HexnumberData));
		if (parseConfig (config, data, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			elektraFree (data);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		elektraPluginSetData (handle, data);
	}

	ElektraKey * cur;
	elektraKeysetRewind (returned);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		if (elektraKeyIsString (cur) && hasHexType (cur))
		{
			status |= convertDecToHex (cur, parentKey);
		}
	}

	return status;
}

int elektraHexnumberClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	HexnumberData * data = elektraPluginGetData (handle);
	if (data)
	{
		elektraKeysetDel (data->integerTypes);
		elektraFree (data);
		elektraPluginSetData (handle, NULL);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * Exports the plugin to be used by Elektra.
 */
Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport (ELEKTRA_HEXNUMBER_PLUGIN_NAME,
				    ELEKTRA_PLUGIN_GET, &elektraHexnumberGet,
				    ELEKTRA_PLUGIN_SET, &elektraHexnumberSet,
				    ELEKTRA_PLUGIN_CLOSE, &elektraHexnumberClose,
				    ELEKTRA_PLUGIN_END);
}

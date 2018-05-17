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
	char ** integerTypes;
	size_t integerTypeCount;
} HexnumberData;

/**
 * Creates a KeySet representing the contract of this plugin.
 */
static KeySet * elektraContract (void)
{
	return ksNew (
		30,
		keyNew ("system/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME, KEY_VALUE, "hexnumber plugin waits for your orders",
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/exports", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/exports/get", KEY_FUNC, elektraHexnumberGet, KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/exports/set", KEY_FUNC, elektraHexnumberSet, KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/exports/close", KEY_FUNC, elektraHexnumberClose, KEY_END),

#include ELEKTRA_README (hexnumber)

		keyNew ("system/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
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
static int convertHexToDec (Key * key, Key * parentKey)
{
	// get hex string from key
	const char * hexValue = keyString (key);

	int errnoSaved = errno;

	// TODO: use supermacro from PR #1850 once merged
	// convert hex string to long long int
	errno = 0;
	char * endPtr;
	unsigned long long int value = strtoull (hexValue, &endPtr, 16);
	if (errno == ERANGE && value == ULLONG_MAX)
	{
		errno = errnoSaved;
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_RANGE, parentKey, "Hexadecimal number %s out of range 0 to %llu", hexValue,
				    ULLONG_MAX);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else if ((errno != 0 && value == 0) || endPtr == hexValue)
	{
		errno = errnoSaved;
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey, "Hexadecimal number '%s' could not be read", hexValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	errno = errnoSaved;

	// convert long long int back to string (formatted as decimal)
	int result = snprintf (NULL, 0, "%llu", value);
	if (result < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey, "Unable to convert '%s' into decimal", hexValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const size_t length = (size_t) result + 1;
	char * decValue = elektraMalloc (length);
	if (!decValue)
	{
		ELEKTRA_MALLOC_ERROR (parentKey, length);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	result = snprintf (decValue, length, "%llu", value);
	if (result < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey, "Unable to convert '%s' into decimal", hexValue);
		elektraFree (decValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// set decimal string in key and set internal metadata
	keySetString (key, decValue);
	keySetMeta (key, ELEKTRA_HEXNUMBER_META_KEY, "1");

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
static int convertDecToHex (Key * key, Key * parentKey)
{
	// get decimal string from key
	const char * decValue = keyString (key);

	int errnoSaved = errno;

	// convert hex string to long long int
	errno = 0;
	char * endPtr;
	unsigned long long int value = strtoull (decValue, &endPtr, 10);
	if (errno == ERANGE && value == ULLONG_MAX)
	{
		errno = errnoSaved;
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_RANGE, parentKey, "Decimal number %s out of range 0 to %llu", decValue,
				    ULLONG_MAX);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else if ((errno != 0 && value == 0) || endPtr == decValue)
	{
		errno = errnoSaved;
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey, "Decimal number '%s' could not be read", decValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	errno = errnoSaved;

	// convert long long int back to string (formatted as hexadecimal)
	const int result = snprintf (NULL, 0, "0x%llx", value);
	if (result < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey, "Unable to convert '%s' into hexadecimal", decValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const size_t length = (size_t) result + 1;
	char * hexValue = elektraMalloc (length);
	if (!hexValue)
	{
		ELEKTRA_MALLOC_ERROR (parentKey, length);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (snprintf (hexValue, length, "0x%llx", value) < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey, "Unable to convert '%s' into hexadecimal", decValue);
		elektraFree (hexValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// set hex string in key and unset internal metadata
	keySetString (key, hexValue);
	keySetMeta (key, ELEKTRA_HEXNUMBER_META_KEY, "0");


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
static bool isHexString (const Key * key)
{
	return elektraStrNCaseCmp (keyString (key), "0x", 2) == 0;
}

/**
 * Checks whether a given Key is specified to have a hexadecimal base.
 *
 * @param key The Key that should be checked.
 *
 * @retval #true if the Key's metadata contains the key /unit/base and its value is hex
 * @retval #false otherwise
 */
static bool isHexUnitBase (const Key * key)
{
	const Key * unitBaseMeta = keyGetMeta (key, "unit/base");
	return elektraStrCmp (keyString (unitBaseMeta), "hex") == 0;
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
static bool hasType (const Key * key, char ** types, size_t typeCount)
{
	const Key * typeMeta = keyGetMeta (key, "type");
	if (!typeMeta)
	{
		return false;
	}

	const char * type = keyString (typeMeta);
	for (size_t i = 0; i < typeCount; ++i)
	{
		if (elektraStrCmp (types[i], type) == 0)
		{
			return true;
		}
	}

	return false;
}

/**
 * Checks whether a given Key's type metadata is #ELEKTRA_HEXNUMBER_META_TYPE.
 *
 * @param key The Key that should be checked.
 *
 * @retval #true if the Key's type metadata is #ELEKTRA_HEXNUMBER_META_TYPE
 * @retval #false otherwise
 */
static bool hasHexType (const Key * key)
{
	const Key * typeMeta = keyGetMeta (key, ELEKTRA_HEXNUMBER_META_KEY);
	return typeMeta && elektraStrCmp (keyString (typeMeta), "1") == 0;
}

/**
 * Parse the configuration as described in README.md
 * @param config the configuration KeySet provided by elektraPluginGetConfig
 * @param data pointer to the struct to store the configuration in
 * @param errorKey Key used to store error information
 */
int parseConfig (KeySet * config, HexnumberData * data, Key * errorKey)
{
	Key * forceKey = ksLookupByName (config, "/force", 0);
	if (forceKey)
	{
		data->forceConversion = true;
	}

	Key * typesKey = keyNew ("/accept/type", KEY_END);
	const ssize_t res = elektraArrayGetStrings (typesKey, config, &data->integerTypes);
	if (res < 0)
	{
		elektraFree (data->integerTypes);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_STATE, errorKey, "Could not parse config! Types not set correctly.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	data->integerTypeCount = (size_t) res;

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
int elektraHexnumberGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/" ELEKTRA_HEXNUMBER_PLUGIN_NAME))
	{
		KeySet * contract = elektraContract ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	HexnumberData * data = elektraPluginGetData (handle);
	if (!data)
	{
		KeySet * config = elektraPluginGetConfig (handle);
		data = elektraCalloc (sizeof (HexnumberData));
		if (parseConfig (config, data, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			elektraFree (data);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		elektraPluginSetData (handle, data);
	}

	Key * cur;
	ksRewind (returned);

	static char * defaultIntegerTypes[] = { "byte",		 "short",     "unsigned_short",    "long",
						"unsigned_long", "long_long", "unsigned_long_long" };
	static const size_t defaultIntegerTypeCount = sizeof (defaultIntegerTypes) / sizeof (const char *);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (!keyIsString (cur))
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
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_FORMAT, parentKey,
						    "Key '%s' has unit/base metadata set as hex but value '%s' does not start with 0x",
						    keyName (cur), keyString (cur));
				status |= ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}
		else if (hexString && (data->forceConversion || hasType (cur, data->integerTypes, data->integerTypeCount) ||
				       hasType (cur, defaultIntegerTypes, defaultIntegerTypeCount)))
		{
			status |= convertHexToDec (cur, parentKey);
		}
	}

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
int elektraHexnumberSet (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	HexnumberData * data = elektraPluginGetData (handle);
	if (!data)
	{
		KeySet * config = elektraPluginGetConfig (handle);
		data = elektraCalloc (sizeof (HexnumberData));
		if (parseConfig (config, data, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			elektraFree (data);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		elektraPluginSetData (handle, data);
	}

	Key * cur;
	ksRewind (returned);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyIsString (cur) && hasHexType (cur))
		{
			status |= convertDecToHex (cur, parentKey);
		}
	}

	return status;
}

int elektraHexnumberClose (Plugin * handle ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	HexnumberData * data = elektraPluginGetData (handle);
	if (data)
	{
		for (size_t i = 0; i < data->integerTypeCount; ++i)
		{
			elektraFree (data->integerTypes[i]);
		}
		elektraFree (data->integerTypes);
		elektraFree (data);
		elektraPluginSetData (handle, NULL);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * Exports the plugin to be used by Elektra.
 */
Plugin * ELEKTRA_PLUGIN_EXPORT (hexnumber)
{
	// clang-format off
	return elektraPluginExport (ELEKTRA_HEXNUMBER_PLUGIN_NAME,
				    ELEKTRA_PLUGIN_GET, &elektraHexnumberGet,
				    ELEKTRA_PLUGIN_SET, &elektraHexnumberSet,
				    ELEKTRA_PLUGIN_CLOSE, &elektraHexnumberClose,
				    ELEKTRA_PLUGIN_END);
}

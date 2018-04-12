/**
 * @file
 *
 * @brief Source for hexnumber plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "hexnumber.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbprivate.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Creates a KeySet representing the contract of this plugin.
 */
static KeySet * elektraContract (void)
{
	return ksNew (30, keyNew ("system/elektra/modules/hexnumber", KEY_VALUE, "hexnumber plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/hexnumber/exports", KEY_END),
		      keyNew ("system/elektra/modules/hexnumber/exports/get", KEY_FUNC, elektraHexnumberGet, KEY_END),
		      keyNew ("system/elektra/modules/hexnumber/exports/set", KEY_FUNC, elektraHexnumberSet, KEY_END),

#include ELEKTRA_README (hexnumber)

		      keyNew ("system/elektra/modules/hexnumber/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

/**
 * Converts a Key with a hexadecimal number value to a Key with a decimal value.
 *
 * @pre The key has to actually contain a hexadecimal number string.
 *
 * @param key The Key whose value should be converted.
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
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	size_t length = (size_t) result + 1;
	char * decValue = elektraMalloc (length);
	if (!decValue)
	{
		ELEKTRA_MALLOC_ERROR (parentKey, length);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	result = snprintf (decValue, length, "%llu", value);
	if (result < 0)
	{
		elektraFree (decValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// set decimal string in key and set type in metadata
	keySetString (key, decValue);
	keySetMeta (key, "type", ELEKTRA_HEXNUMBER_META_TYPE);

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
	int result = snprintf (NULL, 0, "0x%llx", value);
	if (result < 0)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	size_t length = (size_t) result;
	char * hexValue = elektraMalloc (length);
	if (!hexValue)
	{
		ELEKTRA_MALLOC_ERROR (parentKey, length);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	result = snprintf (hexValue, length, "0x%llx", value);
	if (result < 0)
	{
		elektraFree (hexValue);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// set hex string in key
	keySetString (key, hexValue);
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
	return strncasecmp (keyString (key), "0x", 2) == 0;
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
	const Key * typeMeta = keyGetMeta (key, "type");
	return typeMeta && strcmp (keyString (typeMeta), ELEKTRA_HEXNUMBER_META_TYPE) == 0;
}

/**
 * Establish the plugin contract and convert all hexadecimal values in the KeySet to decimal.
 *
 * @note The plugin will attempt to convert ALL values starting with 0x from hexadecimal into decimal.
 * 	 If a value starts with 0x but is not a hexadecimal number the resulting value will be 0!
 *
 * @param handle This parameter stores the configuration of the plugin.
 * @param returned This parameter specifies the key set that this function updates.
 * @param parentKey The function stores information about errors/warnings in this parameter.
 *
 * @retval #ELEKTRA_PLUGIN_STATUS_SUCCESS if any keys were updated
 * @retval #ELEKTRA_PLUGIN_STATUS_NO_UPDATE if \p returned was not modified
 * @retval #ELEKTRA_PLUGIN_STATUS_ERROR on failure
 */
int elektraHexnumberGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/hexnumber"))
	{
		KeySet * contract = elektraContract ();
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Key * cur;
	ksRewind (returned);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (!keyIsString (cur) || !isHexString (cur))
		{
			continue;
		}
		status |= convertHexToDec (cur, parentKey);
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
int elektraHexnumberSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	Key * cur;
	ksRewind (returned);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (!keyIsString (cur) || !hasHexType (cur))
		{
			continue;
		}
		status |= convertDecToHex (cur, parentKey);
	}

	return status;
}

/**
 * Exports the plugin to be used by Elektra.
 */
Plugin * ELEKTRA_PLUGIN_EXPORT (hexnumber)
{
	// clang-format off
	return elektraPluginExport ("hexnumber",
				    ELEKTRA_PLUGIN_GET, &elektraHexnumberGet,
				    ELEKTRA_PLUGIN_SET, &elektraHexnumberSet,
				    ELEKTRA_PLUGIN_END);
}

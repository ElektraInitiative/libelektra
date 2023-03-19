/**
 * @file
 *
 * @brief filter plugin for the Base64 encoding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "base64.h"
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/errors.h>
#include <stdbool.h>
#include <string.h>

/**
 * @brief Unescape a key value starting with two `ELEKTRA_PLUGIN_BASE64_ESCAPE` characters (`@@`).
 *
 * @pre The type of the key value must be string.
 *
 * @param key This parameter contains the key value this function escapes.
 * @param parent The function stores information about errors in this parameter.
 *
 * @retval -1 if the function was unable to unescape the value of `key`
 * @retval 0 if the given key was not modified
 * @retval 1 if the function successfully unescaped `key`
 */
static int unescape (Key * key, Key * parent)
{
	const char * strVal = keyString (key);
	const char escapedPrefix[] = ELEKTRA_PLUGIN_BASE64_ESCAPE ELEKTRA_PLUGIN_BASE64_ESCAPE;

	if (strlen (strVal) < 2 || strncmp (strVal, escapedPrefix, 2) != 0) return 0;

	// Discard the first escape character
	char * unescaped = elektraStrDup (&strVal[1]);
	if (!unescaped)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parent);
		return -1;
	}
	keySetString (key, unescaped);
	elektraFree (unescaped);
	return 1;
}

/**
 * @brief Check if the given key should be decoded by the plugin.
 *
 * @pre The type of the key value must be string.
 *
 * @param key This parameter specifies the key for which this function decides if it should be encoded or not.
 * @param metaMode This parameter specifies if the plugin uses meta mode or not.
 *
 * @retval true if the plugin should decode the given key
 * @retval false otherwise
 */
static bool shouldDecode (Key * key, bool metaMode)
{
	if (metaMode) return keyGetMeta (key, "type") && strcmp (keyValue (keyGetMeta (key, "type")), "binary") == 0;

	const char * strVal = keyString (key);
	return strlen (strVal) >= ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH &&
	       strncmp (strVal, ELEKTRA_PLUGIN_BASE64_PREFIX, ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH) == 0;
}

/**
 * @brief Decode a base64 encoded key value and save the result as binary data in the key.
 *
 * The conversion only happens if the value of the key has type `string` and
 *
 *   1. in escaping mode: the key value starts with `ELEKTRA_PLUGIN_BASE64_PREFIX` (`"@BASE64"`),
 *   2. in meta mode: the key contains the metakey `type` with the value `binary`
 *
 * . If the key value starts with two prefix characters (`@@`) in **escaping mode**, then the function unescapes the value by removing one
 * of the prefix characters.
 *
 * @param key This parameter specifies the key that this function decodes.
 * @param parent The function stores information about errors/warnings in this parameter.
 * @param metaMode This parameter specifies if the plugin uses meta mode or not.
 *
 * @retval -1 if the function was unable to convert or unescape the value of `key`
 * @retval 0 if the given key was not modified
 * @retval 1 if the function successfully modified `key`
 */
static int decode (Key * key, Key * parent, bool metaMode)
{
	if (!keyIsString (key)) return 0;

	if (!shouldDecode (key, metaMode))
	{
		if (metaMode) return 0;
		return unescape (key, parent);
	}

	ELEKTRA_LOG_DEBUG ("Decode binary value");

	kdb_octet_t * buffer;
	size_t bufferLen;
	const char * strVal = keyString (key);
	int result = base64Decode (strVal + (metaMode ? 0 : ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH), &buffer, &bufferLen);
	if (result == 1)
	{
		// Success
		keySetBinary (key, buffer, bufferLen);
	}
	else if (result == -1)
	{
		// Decoding error
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (parent, "Key %s was not Base64 encoded: %s", keyName (key), strVal);
	}
	else if (result == -2)
	{
		// Memory error
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parent);
		return -1;
	}

	elektraFree (buffer);
	return 1;
}

/**
 * @brief Encode a binary key value using base64 encoding and save the result as textual data in the key.
 *
 * @param key This parameter specifies the key that this function encodes.
 * @param parent The function stores information about errors in this parameter.
 * @param metaMode This parameter specifies if the plugin uses meta mode or not.
 *
 * @retval -1 if the function was unable to convert the value of `key`
 * @retval 0 if no conversion has taken place
 * @retval 1 if the function successfully converted the value of `key`
 */
static int encode (Key * key, Key * parent, bool metaMode)
{
	if (!keyIsBinary (key) || (keyGetValueSize (key) == 0 && metaMode)) return 0;

	char * base64 = base64Encode (keyValue (key), (size_t) keyGetValueSize (key));
	if (!base64)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parent);
		return -1;
	}

	if (metaMode)
	{
		keySetString (key, base64);
	}
	else
	{
		const size_t newValLen = strlen (base64) + ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH + 1;
		char * newVal = elektraMalloc (newValLen);
		if (!newVal)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parent);
			elektraFree (base64);
			return -1;
		}
		snprintf (newVal, newValLen, "%s%s", ELEKTRA_PLUGIN_BASE64_PREFIX, base64); //! OCLint (constant conditional operator)
		keySetString (key, newVal);
		elektraFree (newVal);
	}

	elektraFree (base64);

	return 1;
}

/**
 * @brief Escape the prefix character `ELEKTRA_PLUGIN_BASE64_PREFIX` (`@`) in a key value by prefixing it with another `@`.
 *
 * This function only inserts another prefix character if the type of `key` is string.
 *
 * @param key This parameter specifies the key value that this function escapes.
 * @param parent The function stores information about errors in this parameter.
 *
 * @retval -1 if the function was unable to escape the key value
 * @retval 0 if the function did not change the key value
 * @retval 1 if the function successfully escaped the value of `key`
 */
static int escape (Key * key, Key * parent)
{
	if (keyIsString (key) == 0) return 0;

	// escape the prefix character
	const char * strVal = keyString (key);
	const size_t strValLen = strlen (strVal);
	if (strValLen <= 0 || strVal[0] != ELEKTRA_PLUGIN_BASE64_ESCAPE_CHAR) return 0;

	// + 1 for the additional escape character
	// + 1 for the NULL terminator
	char * escapedVal = elektraMalloc (strValLen + 2);
	if (!escapedVal)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parent);
		return -1;
	}

	// add the escape character in front of the original value
	escapedVal[0] = ELEKTRA_PLUGIN_BASE64_ESCAPE_CHAR;
	strncpy (&escapedVal[1], strVal, strValLen + 1); //! OCLint (constant conditional operator)
	keySetString (key, escapedVal);
	elektraFree (escapedVal);
	return 1;
}

/**
 * @brief Check if the plugin should use meta mode for the base64 conversion.
 *
 * @param handle This parameter stores the configuration of the plugin.
 *
 * @retval true if the plugin should use meta mode
 * @retval false if the plugin should use escaping mode
 */
static bool useMetaMode (Plugin * handle)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * metaMode = ksLookupByName (config, "/binary/meta", 0);

	ELEKTRA_LOG ("Using %s mode", metaMode ? "meta" : "escaping");
	return metaMode ? true : false;
}

/**
 * @brief Establish the Elektra plugin contract and decode all Base64 encoded values back to their original binary form.
 *
 * @param handle This parameter stores the configuration of the plugin.
 * @param keySet This parameter specifies the key set that this function updates.
 * @param parent The function stores information about errors/warnings in this parameter.
 *
 * @retval 1 if any keys were updated
 * @retval 0 if `keyset` was not modified
 * @retval -1 on failure
 */
int PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * keySet, Key * parentKey)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/" ELEKTRA_PLUGIN_NAME))
	{
		KeySet * moduleConfig = ksNew (30,
#include "contract.h"
					       KS_END);
		ksAppend (keySet, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	bool metaMode = useMetaMode (handle);

	// base64 decoding
	Key * key;
	int status = 0;

	for (elektraCursor it = 0; status >= 0 && it < ksGetSize (keySet); ++it)
	{
		key = ksAtCursor (keySet, it);
		status |= decode (key, parentKey, metaMode);
	}
	return status;
}

/**
 * @brief Encode all binary values using the Base64 encoding scheme.
 *
 * @param handle This parameter stores the configuration of the plugin.
 * @param keySet This parameter specifies the key set that this function updates.
 * @param parent The function stores information about errors in this parameter.
 *
 * @retval 1 if any keys were updated
 * @retval 0 if `keyset` was not modified
 * @retval -1 on failure
 */
int PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * keySet, Key * parentKey)
{
	Key * key;
	bool metaMode = useMetaMode (handle);
	int status = 0;

	for (elektraCursor it = 0; status >= 0 && it < ksGetSize (keySet); ++it)
	{
		key = ksAtCursor (keySet, it);
		if (!metaMode)
		{
			status |= escape (key, parentKey);
			if (status < 0) break;
		}
		status |= encode (key, parentKey, metaMode);
	}
	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport (ELEKTRA_PLUGIN_NAME, ELEKTRA_PLUGIN_GET, &PLUGIN_FUNCTION (get), ELEKTRA_PLUGIN_SET,
				    &PLUGIN_FUNCTION (set), ELEKTRA_PLUGIN_END);
}

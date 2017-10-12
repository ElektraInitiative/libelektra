/**
 * @file
 *
 * @brief filter plugin for the Base64 encoding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "base64.h"
#include <kdb.h>
#include <kdberrors.h>
#include <string.h>

static int unescape (Key * key, Key * parent)
{
	const char escapedPrefix[] = ELEKTRA_PLUGIN_BASE64_ESCAPE ELEKTRA_PLUGIN_BASE64_ESCAPE;
	const char * strVal = keyString (key);
	if (strlen (strVal) >= 2 && strncmp (strVal, escapedPrefix, 2) == 0)
	{
		// Discard the first escape character
		char * unescaped = strdup (&strVal[1]);
		if (!unescaped)
		{
			ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed");
			return -1;
		}
		keySetString (key, unescaped);
		elektraFree (unescaped);
	}
	return 1;
}

/**
 * @brief Decode a base64 encoded key value and save the result as binary data in the key.
 *
 * The conversion only happens if
 *
 * - the value of the key has type `string`
 * - the key value starts with `ELEKTRA_PLUGIN_BASE64_PREFIX` (`"@BASE64"`).
 *
 * . If the key value starts with two prefix characters (`@@`), then the function unescapes the value by removing one of the prefix
 * characters.
 *
 * @retval -1 if the function was unable to convert or unescape the value of `key`
 * @retval 0 if the given key was not modified
 * @retval 1 if the function successfully modified `key`
 */
static int decode (Key * key, Key * parent)
{
	if (!keyIsString (key)) return 0;

	const char * strVal = keyString (key);
	if (strlen (strVal) < ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH ||
	    strncmp (strVal, ELEKTRA_PLUGIN_BASE64_PREFIX, ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH) != 0)
	{
		return unescape (key, parent);
	}

	ELEKTRA_LOG_DEBUG ("Decode binary value");

	kdb_octet_t * buffer;
	size_t bufferLen;

	int result = PLUGIN_FUNCTION (base64Decode) (strVal + ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH, &buffer, &bufferLen);
	if (result == 1)
	{
		// Success
		keySetBinary (key, buffer, bufferLen);
	}
	else if (result == -1)
	{
		// Decoding error
		ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_BASE64_DECODING, parent, "Not Base64 encoded: %s", strVal);
	}
	else if (result == -2)
	{
		// Memory error
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed");
		return -1;
	}

	elektraFree (buffer);
	return 1;
}

/**
 * @brief Encode a binary key value using base64 encoding and save the result as textual data in the key.
 *
 * @retval -1 if the function was unable to convert the value of `key`
 * @retval 0 if no conversion has taken place
 * @retval 1 if the function successfully converted the value of `key`
 */
static int encode (Key * key, Key * parent)
{
	if (!keyIsBinary (key)) return 0;

	char * base64 = PLUGIN_FUNCTION (base64Encode) (keyValue (key), (size_t)keyGetValueSize (key));
	if (!base64)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed");
		return -1;
	}

	const size_t newValLen = strlen (base64) + ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH + 1;
	char * newVal = elektraMalloc (newValLen);
	if (!newVal)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed");
		elektraFree (base64);
		return -1;
	}
	snprintf (newVal, newValLen, "%s%s", ELEKTRA_PLUGIN_BASE64_PREFIX, base64); //! OCLint (constant conditional operator)

	keySetString (key, newVal);

	elektraFree (newVal);
	elektraFree (base64);

	return 1;
}

/**
 * @brief Escape the prefix character `@` in a key value.
 *
 * This function only replaces the prefix character if the type of `key` is string.
 *
 * @retval -1 if the function was unable to escape the key value
 * @retval 0 if the function did not change the key value
 * @retval 1 if the function successfully converted the value of `key`
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
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed");
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
 * @brief establish the Elektra plugin contract and decode all Base64 encoded values back to their original binary form.
 * @retval 1 on success
 * @retval -1 on failure
 */
int PLUGIN_FUNCTION (get) (Plugin * handle ELEKTRA_UNUSED, KeySet * keySet, Key * parentKey)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parentKey), "system/elektra/modules/" ELEKTRA_PLUGIN_NAME))
	{
		KeySet * moduleConfig = ksNew (30,
#include "contract.h"
					       KS_END);
		ksAppend (keySet, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	// base64 decoding
	Key * key;
	ksRewind (keySet);
	int status = 0;
	while (status >= 0 && (key = ksNext (keySet)))
	{
		status |= decode (key, parentKey);
	}
	return status;
}

/**
 * @brief Encode all binary values using the Base64 encoding scheme.
 * @retval 1 on success
 * @retval -1 on failure
 */
int PLUGIN_FUNCTION (set) (Plugin * handle ELEKTRA_UNUSED, KeySet * keySet, Key * parentKey)
{
	Key * key;

	ksRewind (keySet);
	while ((key = ksNext (keySet)))
	{
		if ((escape (key, parentKey) == -1) || (encode (key, parentKey) == -1)) return -1;
	}
	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (base64)
{
	return elektraPluginExport (ELEKTRA_PLUGIN_NAME, ELEKTRA_PLUGIN_GET, &PLUGIN_FUNCTION (get), ELEKTRA_PLUGIN_SET,
				    &PLUGIN_FUNCTION (set), ELEKTRA_PLUGIN_END);
}

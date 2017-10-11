/**
 * @file
 *
 * @brief filter plugin for the Base64 encoding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif
#include "base64.h"
#include <kdb.h>
#include <kdbassert.h>
#include <kdberrors.h>
#include <stdlib.h>
#include <string.h>

/**
 * @brief Decode a base64 encoded key value and save the result as binary data in the key.
 *
 * The conversion only happens if
 *
 * - the value of the key has type `string`
 * - the key value starts with `ELEKTRA_PLUGIN_BASE64_PREFIX` (`@`).
 *
 * .
 *
 * @retval -1 if the function was unable to convert the value of `key`
 * @retval 0 if no conversion has taken place
 * @retval 1 if the function successfully converted the value of `key`
 */
static int decode (Key * key, Key * parent)
{
	if (!keyIsString (key)) return 0;

	const char * strVal = keyString (key);
	const char * prefix = ELEKTRA_PLUGIN_BASE64_PREFIX;
	const size_t prefixLen = strlen (prefix);
	if (strlen (strVal) < prefixLen || strncmp (strVal, prefix, prefixLen) != 0) return 0;

	ELEKTRA_LOG_DEBUG ("Decode binary value");

	kdb_octet_t * buffer;
	size_t bufferLen;

	int result = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (strVal + prefixLen, &buffer, &bufferLen);
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

	ELEKTRA_LOG_DEBUG ("Decoded data “%s”", (char *)buffer);

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
	const char * prefix = ELEKTRA_PLUGIN_BASE64_PREFIX;
	const size_t prefixLen = strlen (prefix);

	if (!keyIsBinary (key)) return 0;

	char * base64 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) (keyValue (key), (size_t)keyGetValueSize (key));
	if (!base64)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed");
		return -1;
	}

	const size_t newValLen = strlen (base64) + prefixLen + 1;
	char * newVal = elektraMalloc (newValLen);
	if (!newVal)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed");
		elektraFree (base64);
		return -1;
	}
	snprintf (newVal, newValLen, "%s%s", prefix, base64);

	keySetString (key, newVal);

	elektraFree (newVal);
	elektraFree (base64);

	return 1;
}

/**
 * @brief establish the Elektra plugin contract and decode all Base64 encoded values back to their original binary form.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, get) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parentKey), "system/elektra/modules/" ELEKTRA_PLUGIN_NAME))
	{
		KeySet * moduleConfig = ksNew (30,
#include "contract.h"
					       KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	// base64 decoding
	Key * k;
	const char escapedPrefix[] = ELEKTRA_PLUGIN_BASE64_ESCAPE ELEKTRA_PLUGIN_BASE64_ESCAPE;

	ksRewind (ks);
	while ((k = ksNext (ks)))
	{
		int status = decode (k, parentKey);
		if (status == -1) return -1; // Error
		if (status != 0 || !keyIsString (k)) continue;

		const char * strVal = keyString (k);
		if (strlen (strVal) >= 2 && strncmp (strVal, escapedPrefix, 2) == 0)
		{
			// Discard the first escape character
			char * unescaped = strdup (&strVal[1]);
			if (!unescaped)
			{
				ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parentKey, "Memory allocation failed");
				return -1;
			}
			keySetString (k, unescaped);
			elektraFree (unescaped);
		}
	}
	return 1;
}

/**
 * @brief Encode all binary values using the Base64 encoding scheme.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, set) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	Key * k;

	ksRewind (ks);
	while ((k = ksNext (ks)))
	{
		// escape the prefix character
		if (keyIsString (k) == 1)
		{
			const char * strVal = keyString (k);
			const size_t strValLen = strlen (strVal);
			if (strValLen > 0 && strncmp (strVal, ELEKTRA_PLUGIN_BASE64_ESCAPE, 1) == 0)
			{
				// + 1 for the additional escape character
				// + 1 for the NULL terminator
				char * escapedVal = elektraMalloc (strValLen + 2);
				if (!escapedVal)
				{
					ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parentKey, "Memory allocation failed");
					return -1;
				}

				// add the escape character in front of the original value
				escapedVal[0] = ELEKTRA_PLUGIN_BASE64_ESCAPE_CHAR;
				strncpy (&escapedVal[1], strVal, strValLen + 1);
				keySetString (k, escapedVal);
				elektraFree (escapedVal);
			}
		}

		if (encode (k, parentKey) == -1) return -1;
	}
	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (base64)
{
	// clang-format off
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
			ELEKTRA_PLUGIN_GET,   &ELEKTRA_PLUGIN_FUNCTION(ELEKTRA_PLUGIN_NAME_C, get),
			ELEKTRA_PLUGIN_SET,   &ELEKTRA_PLUGIN_FUNCTION(ELEKTRA_PLUGIN_NAME_C, set),
			ELEKTRA_PLUGIN_END);
}

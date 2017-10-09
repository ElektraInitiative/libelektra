/**
 * @file
 *
 * @brief Filter plugin for Base64 encoding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "base666.h"
#include <kdb.h>
#include <kdberrors.h>

/**
 * @brief Decode a base64 encoded key value and save the result as binary data in the key.
 *
 * The conversion only happens if
 *
 * - the value of the key has type `string`
 * - the key contains a metakey `type` with value `binary`
 *
 * .
 *
 * @retval -1 if the function was unable to convert the value of `key`
 * @retval 0 if no conversion has taken place
 * @retval 1 if the function successfully converted the value of `key`
 */
static int decode (Key * key, Key * parent)
{
	const char * strVal = keyString (key);

	if (keyIsString (key) == 0 || !keyGetMeta (key, "type") || strcmp (keyValue (keyGetMeta (key, "type")), "binary")) return 0;

	ELEKTRA_LOG_DEBUG ("Decode binary value");

	kdb_octet_t * buffer;
	size_t bufferLen;

	int result = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (strVal, &buffer, &bufferLen);
	if (result == 1)
	{
		keySetBinary (key, buffer, bufferLen); // Success
	}
	else if (result == -1)
	{
		ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_BASE64_DECODING, parent, "Not Base64 encoded: %s.", strVal); // Decoding error
	}
	else if (result == -2)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed"); // Memory error
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
	if (keyIsBinary (key) == 0) return 0;

	char * base64 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) (keyValue (key), (size_t)keyGetValueSize (key));
	if (!base64)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_MALLOC, parent, "Memory allocation failed");
		return -1;
	}

	keySetString (key, base64);
	elektraFree (base64);

	return 1;
}

// -- Plugin Functions ---------------------------------------------------------------------------------------------------------------------

/** @see elektraDocGet */
int elektraBase666Get (Plugin * handle ELEKTRA_UNUSED, KeySet * keySet, Key * parent)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parent), "system/elektra/modules/base666"))
	{
		KeySet * moduleConfig = ksNew (30,
#include "contract.h"
					       KS_END);
		ksAppend (keySet, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	Key * key;
	ksRewind (keySet);
	int status = 0;
	while (status >= 0 && (key = ksNext (keySet)))
	{
		status |= decode (key, parent);
	}
	return status;
}

/** @see elektraDocSet */
int elektraBase666Set (Plugin * handle ELEKTRA_UNUSED, KeySet * keySet, Key * parent)
{
	Key * key;
	ksRewind (keySet);
	int status = 0;
	while (status >= 0 && (key = ksNext (keySet)))
	{
		status |= encode (key, parent);
	}
	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (base666)
{
	return elektraPluginExport ("base666", ELEKTRA_PLUGIN_GET, &elektraBase666Get, ELEKTRA_PLUGIN_SET, &elektraBase666Set,
				    ELEKTRA_PLUGIN_END);
}

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

int decode (Key * key, Key * parent)
{
	const char * strVal = keyString (key);

	if (keyIsString (key) == 0 || !keyGetMeta (key, "type") || strcmp (keyValue (keyGetMeta (key, "type")), "binary")) return 1;

	ELEKTRA_LOG_DEBUG ("Decode binary value");

	kdb_octet_t * buffer;
	size_t bufferLen;

	int result = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (strVal, &buffer, &bufferLen);
	if (result == 1)
	{
		// success
		keySetBinary (key, buffer, bufferLen);
	}
	else if (result == -1)
	{
		// decoding error
		ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_BASE64_DECODING, parent, "Not Base64 encoded: %s.", strVal);
	}
	else if (result == -2)
	{
		// memory error
		ELEKTRA_SET_ERROR (87, parent, "Memory allocation failed");
		return -1;
	}

	ELEKTRA_LOG_DEBUG ("Decoded data “%s”", (char *)buffer);

	elektraFree (buffer);
	return 1;
}

int encode (Key * key, Key * parent)
{
	if (keyIsBinary (key) == 0 || strcmp (keyValue (keyGetMeta (key, "type")), "binary")) return 1;

	char * base64 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) (keyValue (key), (size_t)keyGetValueSize (key));
	if (!base64)
	{
		ELEKTRA_SET_ERROR (87, parent, "Memory allocation failed");
		return -1;
	}

	keySetString (key, base64);
	elektraFree (base64);

	return 1;
}

/**
 * @brief establish the Elektra plugin contract and decode all Base64 encoded values back to their original binary form.
 * @retval 1 on success
 * @retval -1 on failure
 */
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

	// base64 decoding
	Key * key;

	ksRewind (keySet);
	int status = 0;
	while (status >= 0 && (key = ksNext (keySet)))
	{
		status = decode (key, parent);
	}
	return status;
}

/**
 * @brief Encode all binary values using the Base64 encoding scheme.
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraBase666Set (Plugin * handle ELEKTRA_UNUSED, KeySet * keySet, Key * parent)
{
	Key * key;

	ksRewind (keySet);
	int status = 0;
	while (status >= 0 && (key = ksNext (keySet)))
	{
		status = encode (key, parent);
	}
	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (base666)
{
	return elektraPluginExport ("base666", ELEKTRA_PLUGIN_GET, &elektraBase666Get, ELEKTRA_PLUGIN_SET, &elektraBase666Set,
				    ELEKTRA_PLUGIN_END);
}

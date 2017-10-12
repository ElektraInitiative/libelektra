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
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
static inline KeySet * base666Contract (void)
{
	return ksNew (30, keyNew ("system/elektra/modules/base666", KEY_VALUE, "base666 plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/base666/exports", KEY_END),
		      keyNew ("system/elektra/modules/base666/exports/get", KEY_FUNC, elektraBase666Get, KEY_END),
		      keyNew ("system/elektra/modules/base666/exports/set", KEY_FUNC, elektraBase666Set, KEY_END),
#include ELEKTRA_README (base666)
		      keyNew ("system/elektra/modules/base666/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),

		      KS_END);
}

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
	if (keyIsString (key) == 0 || !keyGetMeta (key, "type") || strcmp (keyValue (keyGetMeta (key, "type")), "binary")) return 0;
	const char * strVal = keyString (key);

	ELEKTRA_LOG_DEBUG ("Decode binary value");

	kdb_octet_t * buffer;
	size_t bufferLen;

	int result = PLUGIN_FUNCTION (base64Decode) (strVal, &buffer, &bufferLen);
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

	char * base64 = PLUGIN_FUNCTION (base64Encode) (keyValue (key), (size_t)keyGetValueSize (key));
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
	if (!strcmp (keyName (parent), "system/elektra/modules/base666"))
	{
		KeySet * contract = base666Contract ();
		ksAppend (keySet, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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

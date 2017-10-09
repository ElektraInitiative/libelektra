/**
 * @file
 *
 * @brief Filter plugin for Base64 encoding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif
#include "base666.h"
#include <kdb.h>
#include <kdbassert.h>
#include <kdberrors.h>
#include <stdlib.h>
#include <string.h>

/**
 * @brief establish the Elektra plugin contract and decode all Base64 encoded values back to their original binary form.
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraBase666Get (Plugin * handle ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parentKey), "system/elektra/modules/base666"))
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

	ksRewind (ks);
	while ((k = ksNext (ks)))
	{
		if (keyIsString (k) == 1)
		{
			const char * strVal = keyString (k);

			if (keyGetMeta (k, "type") && !strcmp (keyValue (keyGetMeta (k, "type")), "binary"))
			{
				ELEKTRA_LOG_DEBUG ("Decode binary value");

				kdb_octet_t * buffer;
				size_t bufferLen;

				int result = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (strVal, &buffer, &bufferLen);
				if (result == 1)
				{
					// success
					keySetBinary (k, buffer, bufferLen);
				}
				else if (result == -1)
				{
					// decoding error
					ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_BASE64_DECODING, parentKey, "Not Base64 encoded: %s.",
							      strVal);
				}
				else if (result == -2)
				{
					// memory error
					ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
					return -1;
				}

				ELEKTRA_LOG_DEBUG ("Decoded data “%s”", (char *)buffer);

				elektraFree (buffer);
			}
		}
	}
	return 1;
}

/**
 * @brief Encode all binary values using the Base64 encoding scheme.
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraBase666Set (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	Key * k;

	ksRewind (ks);
	while ((k = ksNext (ks)))
	{
		// Base 64 encoding
		if (keyIsBinary (k) == 1 && !strcmp (keyValue (keyGetMeta (k, "type")), "binary"))
		{
			char * base64 =
				ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) (keyValue (k), (size_t)keyGetValueSize (k));
			if (!base64)
			{
				ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
				return -1;
			}

			keySetString (k, base64);
			elektraFree (base64);
		}
	}
	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (base666)
{
	// clang-format off
	return elektraPluginExport("base666",
			ELEKTRA_PLUGIN_GET, &elektraBase666Get,
			ELEKTRA_PLUGIN_SET, &elektraBase666Set,
			ELEKTRA_PLUGIN_END);
}

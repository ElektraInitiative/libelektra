/**
 * @file
 *
 * @brief filter plugin for the Base64 encoding
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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
 * @brief establish the Elektra plugin contract and decode all Base64 encoded values back to their original binary form.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, get) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
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

	const char * prefix = ELEKTRA_PLUGIN_BASE64_PREFIX;
	const size_t prefixLen = strlen (prefix);
	const char escapedPrefix[] = ELEKTRA_PLUGIN_BASE64_ESCAPE ELEKTRA_PLUGIN_BASE64_ESCAPE;

	ksRewind (ks);
	while ((k = ksNext (ks)))
	{
		if (keyIsString (k) == 1)
		{
			const char * strVal = keyString (k);

			if (strlen (strVal) >= prefixLen && strncmp (strVal, prefix, prefixLen) == 0)
			{
				// Base64 encoding
				kdb_octet_t * buffer;
				size_t bufferLen;

				int result = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (strVal + prefixLen, &buffer,
													    &bufferLen);
				if (result == 1)
				{
					// success
					keySetBinary (k, buffer, bufferLen);
				}
				else if (result == -1)
				{
					// decoding error
					ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_BASE64_DECODING, parentKey,
							      "Not Base64 encoded: %s. Maybe use a different prefix?", strVal);
				}
				else if (result == -2)
				{
					// memory error
					ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
					return -1;
				}

				elektraFree (buffer);
			}
			else if (strlen (strVal) >= 2 && strncmp (strVal, escapedPrefix, 2) == 0)
			{
				// discard the first escape character
				char * unescaped = strdup (&strVal[1]);
				if (!unescaped)
				{
					ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
					return -1;
				}
				keySetString (k, unescaped);
				elektraFree (unescaped);
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
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, set) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	Key * k;

	const char * prefix = ELEKTRA_PLUGIN_BASE64_PREFIX;
	const size_t prefixLen = strlen (prefix);

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
					ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
					return -1;
				}

				// add the escape character in front of the original value
				escapedVal[0] = ELEKTRA_PLUGIN_BASE64_ESCAPE_CHAR;
				strncpy (&escapedVal[1], strVal, strValLen + 1);
				keySetString (k, escapedVal);
				elektraFree (escapedVal);
			}
		}

		// Base 64 encoding
		if (keyIsBinary (k) == 1)
		{
			char * base64 =
				ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) (keyValue (k), (size_t)keyGetValueSize (k));
			if (!base64)
			{
				ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
				return -1;
			}

			const size_t newValLen = strlen (base64) + prefixLen + 1;
			char * newVal = elektraMalloc (newValLen);
			if (!newVal)
			{
				ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation failed");
				elektraFree (base64);
				return -1;
			}
			snprintf (newVal, newValLen, "%s%s", prefix, base64);

			keySetString (k, newVal);

			elektraFree (newVal);
			elektraFree (base64);
		}
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

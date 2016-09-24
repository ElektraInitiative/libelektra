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

static const char alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
#define ALPHABET_LENGTH (sizeof (alphabet) - 1)
static const char padding = '=';

/**
 * @brief encodes arbitrary binary data using the Base64 encoding scheme (RFC4648)
 * @param input holds the data to be encoded
 * @param inputLength tells how many bytes the input buffer is holding.
 * @returns an allocated string holding the Base64 encoded input data or NULL if the string can not be allocated. Must be freed by the caller.
 */
char * ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) (const kdb_octet_t * input, const size_t inputLength)
{
	size_t encodedLength = 0;
	if (inputLength % 3 == 0)
	{
		encodedLength = (inputLength / 3 * 4) + 1;
	}
	else
	{
		encodedLength = ((inputLength + (3 - (inputLength % 3))) / 3 * 4) + 1;
	}
	ELEKTRA_ASSERT (encodedLength > 0, "Base64 output array size smaller or equal to 0.");

	size_t out = 0;
	char * encoded = elektraMalloc (encodedLength);
	if (!encoded) return NULL;

	for (size_t i = 0; i < inputLength; i += 3)
	{
		if (inputLength - i < 3)
		{
			// padding required
			kdb_octet_t padded[3] = { 0 };
			memcpy (padded, input + i, inputLength - i);

			encoded[out++] = alphabet[padded[0] >> 2];
			encoded[out++] = alphabet[((padded[0] << 4) + (padded[1] >> 4)) & 0x3f];

			if (inputLength - i == 2)
			{
				// 2 octets available in input
				encoded[out++] = alphabet[((padded[1] << 2) + (padded[2] >> 6)) & 0x3f];
				encoded[out++] = padding;
			}
			else
			{
				// 1 octet available in input
				encoded[out++] = padding;
				encoded[out++] = padding;
			}
		}
		else
		{
			// no padding required
			encoded[out++] = alphabet[input[i] >> 2];
			encoded[out++] = alphabet[((input[i] << 4) + (input[i + 1] >> 4)) & 0x3f];
			encoded[out++] = alphabet[((input[i + 1] << 2) + (input[i + 2] >> 6)) & 0x3f];
			encoded[out++] = alphabet[input[i + 2] & 0x3f];
		}
	}
	encoded[out++] = '\0';
	return encoded;
}

/**
 * @brief lookup Base64 character in the alphabet and return the index.
 * @param c the character to look up
 * @param errorFlag set to 1 in case of error
 * @returns the index in the Base64 alphabet or 0 if the padding character was detected.
 */
static kdb_octet_t getBase64Index (const char c, int * errorFlag)
{
	if (c == padding) return 0;

	for (kdb_octet_t i = 0; i < ALPHABET_LENGTH; i++)
	{
		if (alphabet[i] == c) return i;
	}
	*errorFlag = 1;
	return 0;
}

/**
 * @brief decodes Base64 encoded data.
 * @param input holds the Base64 encoded data string
 * @param output will be set to an allocated buffer holding the decoded data or NULL if the allocation failed. Must be freed by the caller on success.
 * @param outputLength will be set to the amount of decoded bytes.
 * @retval 1 on success
 * @retval -1 if the provided string has not been encoded with Base64
 * @retval -2 if the output buffer allocation failed
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Decode) (const char * input, kdb_octet_t ** output, size_t * outputLength)
{
	const size_t inputLen = strlen (input);
	if (inputLen == 0 || (inputLen == 1 && input[0] == '\0'))
	{
		*output = NULL;
		*outputLength = 0;
		return 1;
	}

	if (inputLen % 4 != 0)
	{
		*output = NULL;
		return -1;
	}

	*outputLength = inputLen / 4 * 3;
	if (input[inputLen - 1] == padding) (*outputLength)--;
	if (input[inputLen - 2] == padding) (*outputLength)--;

	*output = elektraMalloc (*outputLength);
	if (!(*output)) return -2;

	size_t i = 0;
	size_t outputIndex = 0;

	for (i = 0; i < inputLen; i += 4)
	{
		int errorFlag = 0;
		const kdb_octet_t b0 = getBase64Index (input[i], &errorFlag);
		const kdb_octet_t b1 = getBase64Index (input[i + 1], &errorFlag);
		const kdb_octet_t b2 = getBase64Index (input[i + 2], &errorFlag);
		const kdb_octet_t b3 = getBase64Index (input[i + 3], &errorFlag);

		if (errorFlag)
		{
			// invalid character detected in input string
			elektraFree (*output);
			*output = NULL;
			return -1;
		}

		(*output)[outputIndex++] = (b0 << 2) + (b1 >> 4);
		if (input[i + 2] != padding)
		{
			(*output)[outputIndex++] = (b1 << 4) + (b2 >> 2);
		}
		if (input[i + 3] != padding)
		{
			(*output)[outputIndex++] = (b2 << 6) + b3;
		}
	}
	return 1;
}

/**
 * @brief establish the Elektra plugin contract and decode all Base64 encoded values back to their original binary form.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, get) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
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

				int result = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Decode) (strVal + prefixLen, &buffer,
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
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, set) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
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
				ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) (keyValue (k), (size_t)keyGetValueSize (k));
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
			ELEKTRA_PLUGIN_GET,   &ELEKTRA_PLUGIN_FUNCTION(ELEKTRA_PLUGIN_NAME, get),
			ELEKTRA_PLUGIN_SET,   &ELEKTRA_PLUGIN_FUNCTION(ELEKTRA_PLUGIN_NAME, set),
			ELEKTRA_PLUGIN_END);
}

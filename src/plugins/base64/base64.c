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
#include <kdberrors.h>
#include <stdlib.h>
#include <string.h>

static const char * alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const char padding = '=';

/**
 * @brief encodes arbitrary binary data using the Base64 encoding scheme (RFC4648)
 * @param input holds the data to be encoded
 * @param inputLength tells how many bytes the input buffer is holding.
 * @returns an allocated string holding the Base64 encoded input data or NULL if the string can not be allocated. Must be freed by the caller.
 */
char * ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) (kdb_octet_t * input, size_t inputLength)
{
	size_t out = 0;
	char * encoded = elektraMalloc (2 * inputLength + 1);
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
 * @brief decodes Base64 encoded data.
 * @param input holds the Base64 encoded data string
 * @param output will be set to an allocated buffer holding the decoded data. Must be freed by the caller.
 * @param outputLength will be set to the amount of decoded bytes.
 * @retval 1 on success
 * @retval -1 if the provided string has not been encoded with Base64
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Decode) (const char * input, kdb_octet_t ** output, size_t * outputLength)
{
	// TODO implement decoding
	return 1;
}

/**
 * @brief establish the Elektra plugin contract and decrypt the file provded at parentKey using GPG.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, get) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
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

	// TODO base64 decoding
	return 1;
}

/**
 * @brief Encrypt the file provided at parentKey using GPG.
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, set) (Plugin * handle, KeySet * ks ELEKTRA_UNUSED, Key * parentKey)
{
	// TODO base64 encoding
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

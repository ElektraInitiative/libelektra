/**
 * @file
 *
 * @brief helper functions for the crypto plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "helper.h"
#include "crypto.h"
#include "gpg.h"
#include <kdberrors.h>
#include <stdlib.h>

/**
* @brief read the encrypted password form the configuration and decrypt it.
* @param errorKey holds an error description in case of failure.
* @param config holds the plugin configuration.
* @returns the decrypted master password as (Elektra) Key or NULL in case of error. Must be freed by the caller.
*/
Key * elektraCryptoGetMasterPassword (Key * errorKey, KeySet * config)
{
	Key * master = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD, 0);
	if (!master)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s in plugin configuration",
				    ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD);
		return NULL;
	}
	Key * msg = keyDup (master);
	if (elektraCryptoGpgDecryptMasterPassword (config, errorKey, msg) != 1)
	{
		keyDel (msg);
		return NULL;
	}
	return msg;
}

/**
 * @brief read the desired iteration count from config
 * @param config KeySet holding the plugin configuration
 * @returns the number of iterations for the key derivation function
 */
kdb_unsigned_long_t elektraCryptoGetIterationCount (KeySet * config)
{
	Key * k = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_ITERATION_COUNT, 0);
	if (k)
	{
		const kdb_unsigned_long_t iterations = strtoul (keyString (k), NULL, 10);
		if (iterations > 0)
		{
			return iterations;
		}
	}
	return ELEKTRA_CRYPTO_DEFAULT_ITERATION_COUNT;
}

static short hexChar2Short (char c)
{
	if (c == '0') return 0;
	if (c == '1') return 1;
	if (c == '2') return 2;
	if (c == '3') return 3;
	if (c == '4') return 4;
	if (c == '5') return 5;
	if (c == '6') return 6;
	if (c == '7') return 7;
	if (c == '8') return 8;
	if (c == '9') return 9;
	if (c == 'A' || c == 'a') return 10;
	if (c == 'B' || c == 'b') return 11;
	if (c == 'C' || c == 'c') return 12;
	if (c == 'D' || c == 'd') return 13;
	if (c == 'E' || c == 'e') return 14;
	if (c == 'F' || c == 'f') return 15;
	return -1;
}

/**
 * @brief converts a string in hexadecimal format into binary data.
 * @param errorKey holds an error description if NULL is returned.
 * @param hexBuffer holds the string with hexadecimal digitis.
 * @param output points to an allocated byte array holding the binary data. Must be freed by the caller. If set to NULL, errorKey holds an error description.
 * @param outputLen holds the address where the number of output bytes is written to. Ignored if set to NULL.
 * @returns
 */
void elektraCryptoHex2Bin (Key * errorKey, const char * hexBuffer, kdb_octet_t ** output, size_t * outputLen)
{
	const size_t length = strlen (hexBuffer);

	// validate length of hexBuffer
	if (length % 2 != 0 || length == 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
				    "failed to interpret \'%s\' as hexadecimal string: odd length", hexBuffer);
		return;
	}

	*output = elektraMalloc (length / 2);
	if (!(*output))
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return;
	}

	for (size_t i = 0; i < length; i += 2)
	{
		const short msb = hexChar2Short (hexBuffer[i]);
		const short lsb = hexChar2Short (hexBuffer[i + 1]);

		if (msb == -1 || lsb == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
					    "failed to interpret \'%s\' as hexadecimal string: invalid character", hexBuffer);
			elektraFree (*output);
			*output = NULL;
			return;
		}

		(*output)[i / 2] = 16 * msb;
		(*output)[i / 2] += lsb;
	}

	if (outputLen) *outputLen = length / 2;
}

/**
 * @brief converts binary data into a hexadecimal string representation.
 * @param errorKey holds an error description if NULL is returned.
 * @param buffer holds the binary data to be converted
 * @param length is the number of bytes to be converted
 * @returns an allocated character array holding the hex-representation of buffer. Must be freed by the caller. If NULL is returned, errorKey holds an error description.
 */
char * elektraCryptoBin2Hex (Key * errorKey, const kdb_octet_t * buffer, const size_t length)
{
	// every byte is represented by 2 hexadecimal digits, thus 2 * length + a NULL terminator
	char * hexBuffer = elektraMalloc (2 * length + 1);
	if (!hexBuffer)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return NULL;
	}

	for (size_t i = 0; i < length; i++)
	{
		snprintf (&hexBuffer[2 * i], 3, "%02X", buffer[i]);
	}
	return hexBuffer;
}

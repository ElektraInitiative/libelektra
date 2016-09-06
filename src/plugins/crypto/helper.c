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
#include <kdberrors.h>
#include <stdlib.h>

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
	if (c >= '0' && c <= '9') return c - '0';
	if (c >= 'A' && c <= 'F') return (c - 'A') + 10;
	if (c >= 'a' && c <= 'f') return (c - 'a') + 10;
	return -1;
}

/**
 * @brief converts a string in hexadecimal format into binary data.
 * @param errorKey holds an error description if NULL is returned.
 * @param hexBuffer holds the string with hexadecimal digitis.
 * @returns an allocated byte array holding the binary data. Must be freed by the caller. If NULL is returned, errorKey holds an error description.
 */
kdb_octet_t * elektraCryptoHex2Bin (Key * errorKey, const char * hexBuffer)
{
	kdb_octet_t * buffer = NULL;
	const size_t length = strlen (hexBuffer);

	// validate length of hexBuffer
	if (length % 2 != 0 || length == 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "failed to interpret %s as hexadecimal string",
				    hexBuffer);
		return NULL;
	}

	buffer = elektraMalloc (length / 2);
	if (!buffer)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return NULL;
	}

	for (size_t i = 0; i < length; i += 2)
	{
		const short msb = hexChar2Short (hexBuffer[i]);
		const short lsb = hexChar2Short (hexBuffer[i + 1]);

		if (msb == -1 || lsb == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "failed to interpret %s as hexadecimal string",
					    hexBuffer);
			elektraFree (buffer);
			return NULL;
		}

		buffer[i / 2] = 16 * msb;
		buffer[i / 2] += lsb;
	}
	return buffer;
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

/**
 * @brief normalizes a string to ASCII characters between 0x20 and 0x7E.
 * 0x00-terminates the string at position (length - 1).
 *
 * @param buffer the buffer holding the content to be normalized
 * @param length the amount of allocated memory for buffer
 */
void elektraCryptoNormalizeRandomString (kdb_octet_t * buffer, const kdb_unsigned_short_t length)
{
	for (int i = 0; i < (length - 1); i++)
	{
		if (buffer[i] > 0x7E)
		{
			buffer[i] = buffer[i] % 0x7E;
		}
		if (buffer[i] < 0x20)
		{
			buffer[i] = buffer[i] + 0x20;
		}
	}
	buffer[length - 1] = 0x00;
}

/**
 * @file
 *
 * @brief helper functions for dealing with random values
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "rand_helper.h"
#include "crypto.h"
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

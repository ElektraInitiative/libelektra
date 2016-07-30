/**
 * @file
 *
 * @brief helper functions for dealing with random values
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "rand_helper.h"

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

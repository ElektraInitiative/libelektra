/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */


#include "kdbprivate.h"
#include <stdlib.h>

/* Hash function
 * By Bob Jenkins, May 2006
 * http://burtleburtle.net/bob/c/lookup3.c
 * Original name: hashword
 */
uint32_t opmphmHashfunction (const uint32_t * key, size_t length, uint32_t initval)
{
	uint32_t a, b, c;

	a = b = c = 0xdeadbeef + (((uint32_t)length) << 2) + initval;

	while (length > 3)
	{
		a += key[0];
		b += key[1];
		c += key[2];
		mix (a, b, c);
		length -= 3;
		key += 3;
	}


	switch (length)
	{
	case 3:
		c += key[2];
	case 2:
		b += key[1];
	case 1:
		a += key[0];
		final (a, b, c);
	case 0:
		break;
	}
	return c;
}

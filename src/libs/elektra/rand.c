/**
 * @file
 *
 * @brief Rand for Elektra.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "kdbrand.h"
#include <kdbassert.h>

/**
 * Non cryptographic pseudo random number generator
 * By Ray Gardner
 * www8.cs.umu.se/~isak/snippets/rg_rand.c
 *
 * based on "Random Number Generators: Good Ones Are Hard to Find",
 * S.K. Park and K.W. Miller, Communications of the ACM 31:10 (Oct 1988),
 * and "Two Fast Implementations of the 'Minimal Standard' Random
 * Number Generator", David G. Carta, Comm. ACM 33, 1 (Jan 1990), p. 87-88
 *
 * linear congruential generator f(z) = 16807 z mod (2 ** 31 - 1)
 *
 * uses L. Schrage's method to avoid overflow problems
 *
 * Make sure the initial seed is: 0 < seed < ELEKTRARANDMAX
 *
 * @param seed a pointer to the seed
 *
 */
void elektraRand (int32_t * seed)
{
	ELEKTRA_ASSERT (seed, "seed is NULL");
	ELEKTRA_ASSERT (*seed, "seed is 0");
	ELEKTRA_ASSERT (*seed <= ELEKTRARANDMAX, "seed is equal or bigger than ELEKTRARANDMAX");
	uint32_t lo, hi;
	lo = 16807 * (int32_t) (*seed & 0xFFFF);
	hi = 16807 * (int32_t) ((uint32_t)*seed >> 16);
	lo += (hi & 0x7FFF) << 16;
	if (lo > ELEKTRARANDMAX)
	{
		lo &= ELEKTRARANDMAX;
		++lo;
	}
	lo += hi >> 15;
	if (lo > ELEKTRARANDMAX)
	{
		lo &= ELEKTRARANDMAX;
		++lo;
	}
	*seed = (int32_t)lo;
}

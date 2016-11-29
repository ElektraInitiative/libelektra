/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */


#include <kdbassert.h>
#include <kdbopmphm.h>

/* Hash function
 * By Bob Jenkins, May 2006
 * http://burtleburtle.net/bob/c/lookup3.c
 * Original name: hashlitte (the little endian part)
 * For now assuming little endian maschine
 */
uint32_t opmphmHashfunction (const void * key, size_t length, uint32_t initval)
{
	uint32_t a, b, c;
	a = b = c = 0xdeadbeef + ((uint32_t)length) + initval;
	const uint32_t * k = (const uint32_t *)key;
	while (length > 12)
	{
		a += k[0];
		b += k[1];
		c += k[2];
		OPMPHM_HASHFUNCTION_MIX (a, b, c)
		length -= 12;
		k += 3;
	}
	switch (length)
	{
	case 12:
		c += k[2];
		b += k[1];
		a += k[0];
		break;
	case 11:
		c += k[2] & 0xffffff;
		b += k[1];
		a += k[0];
		break;
	case 10:
		c += k[2] & 0xffff;
		b += k[1];
		a += k[0];
		break;
	case 9:
		c += k[2] & 0xff;
		b += k[1];
		a += k[0];
		break;
	case 8:
		b += k[1];
		a += k[0];
		break;
	case 7:
		b += k[1] & 0xffffff;
		a += k[0];
		break;
	case 6:
		b += k[1] & 0xffff;
		a += k[0];
		break;
	case 5:
		b += k[1] & 0xff;
		a += k[0];
		break;
	case 4:
		a += k[0];
		break;
	case 3:
		a += k[0] & 0xffffff;
		break;
	case 2:
		a += k[0] & 0xffff;
		break;
	case 1:
		a += k[0] & 0xff;
		break;
	case 0:
		return c;
	}
	OPMPHM_HASHFUNCTION_FINAL (a, b, c);
	return c;
}

/*
 * This Random function comes from:
 * S. Park & K. Miller
 * Random Number Generator: Good ones are Hard to find
 * http://www.firstpr.com.au/dsp/rand31/p1192-park.pdf
 * 1988
 */
uint32_t opmphmRandom (unsigned int * seedp)
{
	ELEKTRA_ASSERT (seedp != NULL, "NULL pointer passed");
	*seedp = (16807 * *seedp) % 2147483647;
	return rand_r (seedp);
}

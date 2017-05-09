/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */


#include <kdbassert.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbopmphm.h>

#include <string.h> //strlen

double opmphmRatio = 2;
static size_t opmphmGetWidth (size_t n);

/**
 * @brief Prepares data for the opmphm build.
 *
 * The opmphmInit () initializes the seeds and sets the k-tuple of each key with the key name in order.
 * The order gets sorted with radix sort by the k-tuples, the first (h[0]) is the less significant position.
 * The sorted Order is checked for duplicates.
 *
 * @param opmphm the OPMPHM
 * @param init the initialisation data
 * @param order storage for the k-tuple
 * @param n the number of keys
 *
 * @retval OpmphmOrder ** is the sorted order
 * @retval NULL on duplicate
 */
OpmphmOrder ** opmphmInit (Opmphm * opmphm, OpmphmInit * init, OpmphmOrder * order, size_t n)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	ELEKTRA_ASSERT (init != NULL, "passed init is a Null Pointer");
	ELEKTRA_ASSERT (order != NULL, "passed order is a Null Pointer");
	ELEKTRA_ASSERT (n > 0, "passed n <= 0");
	ELEKTRA_ASSERT (opmphmRatio >= 1, "opmphmRatio less 1");
	// set the seeds, for the hash function
	for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
	{
		opmphm->opmphmHashFunctionSeeds[t] = opmphmRandom (&(init->initSeed));
	}
	OpmphmOrder ** sortOrder = elektraMalloc (sizeof (OpmphmOrder *) * n);
	if (!sortOrder) return NULL;
	size_t w = opmphmGetWidth (opmphmRatio * n);
	// fill sortOrder struct
	for (size_t i = 0; i < n; ++i)
	{
		sortOrder[i] = &order[i];
#ifndef OPMPHM_TEST
		// set the resulting hash values for each key
		const char * name = init->getString (init->data[i]);
		for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
		{
			sortOrder[i]->h[t] =
				opmphmHashfunction ((const uint32_t *)name, strlen (name), opmphm->opmphmHashFunctionSeeds[t]) % w;
		}
#endif
	}
	// sort elements in sortOrder with radixsort
	// determine the maximum bucket capacity
	size_t numberOfElements = 1;
	for (unsigned int t = 1; t < OPMPHMTUPLE; ++t)
	{
		numberOfElements = numberOfElements * w;
	}
	ELEKTRA_LOG ("OPMPHM w= %lu n= %lu opmphmRatio= %f radix numberOfElements per Bucket= %lu", w, n, opmphmRatio, numberOfElements);
	OpmphmOrder ** buckets = elektraMalloc (sizeof (OpmphmOrder *) * w * numberOfElements);
	size_t * bucketsCount = elektraMalloc (sizeof (size_t) * w);
	if (!buckets || !bucketsCount)
	{
		free (sortOrder);
		return NULL;
	}
	for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
	{
		for (size_t i = 0; i < w; ++i)
		{
			bucketsCount[i] = 0;
		}
		// partition
		for (size_t i = 0; i < n; ++i)
		{
			if (bucketsCount[sortOrder[i]->h[t]] + 1 == numberOfElements + 1)
			{
				// duplicate
				elektraFree (buckets);
				elektraFree (bucketsCount);
				elektraFree (sortOrder);
				return NULL;
			}
			else
			{
				buckets[sortOrder[i]->h[t] * numberOfElements + bucketsCount[sortOrder[i]->h[t]]] = sortOrder[i];
				++bucketsCount[sortOrder[i]->h[t]];
			}
		}
		// collection
		size_t index = 0;
		for (size_t i = 0; i < w; ++i)
		{
			for (size_t j = 0; j < bucketsCount[i]; ++j)
			{
				sortOrder[index] = buckets[i * numberOfElements + j];
				++index;
			}
		}
	}
	elektraFree (buckets);
	elektraFree (bucketsCount);
	// check for duplicates
	for (size_t i = 0; i < n - 1; ++i)
	{
		bool match = true;
		for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
		{
			if (sortOrder[i]->h[t] != sortOrder[i + 1]->h[t])
			{
				match = false;
				break;
			}
		}
		if (match)
		{
			// duplicate
			elektraFree (sortOrder);
			return NULL;
		}
	}
	return sortOrder;
}

static size_t opmphmGetWidth (size_t n)
{
	ELEKTRA_ASSERT (n > 0, "passed n <= 0");
	size_t w = 1;
	size_t space = 0;
	while (space < n)
	{
		++w;
		space = 1;
		for (unsigned int noeiop = 0; noeiop < OPMPHMTUPLE; ++noeiop)
		{
			space = space * w;
		}
	}
	return w;
}

/**
 * Hash function
 * By Bob Jenkins, May 2006
 * http://burtleburtle.net/bob/c/lookup3.c
 * Original name: hashlitte (the little endian part)
 * For now assuming little endian machine
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

/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include <kdbassert.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbopmphm.h>
#include <kdbrand.h>

#include <string.h> //strlen

double opmphmRatio = 2;
static size_t opmphmGetWidth (size_t n);

/**
 * @brief Maps data for the opmphm build.
 *
 * The opmphmMapping () initializes the seeds and sets the k-tuple of each key with the key name in order.
 * The order gets sorted with radix sort by the k-tuples, the first (h[0]) is the most significant position.
 * The sorted Order is checked for duplicates.
 *
 * @param opmphm the OPMPHM
 * @param init the initialisation data
 * @param order storage for the k-tuple
 * @param sortOrder sorted pointers for the k-tuple
 * @param n the number of keys
 *
 * @retval -1 memory error
 * @retval 0 no duplicate
 * @retval 1 duplicate
 */
int opmphmMapping (Opmphm * opmphm, OpmphmInit * init, OpmphmOrder * order, OpmphmOrder ** sortOrder, size_t n)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	ELEKTRA_ASSERT (init != NULL, "passed init is a Null Pointer");
	ELEKTRA_ASSERT (order != NULL, "passed order is a Null Pointer");
	ELEKTRA_ASSERT (sortOrder != NULL, "passed sortOrder is a Null Pointer");
	ELEKTRA_ASSERT (n > 0, "passed n <= 0");
	ELEKTRA_ASSERT (opmphmRatio >= 1, "opmphmRatio less 1");
	// set the seeds, for the hash function
	for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
	{
		elektraRand (&(init->initSeed));
		opmphm->opmphmHashFunctionSeeds[t] = init->initSeed;
	}
	const size_t w = opmphmGetWidth (opmphmRatio * n);
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
	OpmphmOrder ** buckets = elektraMalloc (sizeof (OpmphmOrder *) * w * numberOfElements);
	if (!buckets)
	{
		return -1;
	}
	size_t * bucketsCount = elektraMalloc (sizeof (size_t) * w);
	if (!bucketsCount)
	{
		elektraFree (buckets);
		return -1;
	}
	for (int t = OPMPHMTUPLE - 1; t >= 0; --t)
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
				return 1;
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
			return 1;
		}
	}
	return 0;
}

/**
 * @brief Prepares data for the opmphm mapping and build.
 *
 * The opmphmInit () initializes the outputBase, allocates memory for the sortOrder and transforms the desired hash map return value
 * in the internal representation.
 *
 * @param opmphm the OPMPHM
 * @param init the initialisation data
 * @param order storage for the k-tuple
 * @param n the number of keys
 *
 * @retval OpmphmOrder ** success
 * @retval NULL memory error
 */
OpmphmOrder ** opmphmInit (Opmphm * opmphm, OpmphmInit * init, OpmphmOrder * order, size_t n)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	ELEKTRA_ASSERT (init != NULL, "passed init is a Null Pointer");
	ELEKTRA_ASSERT (order != NULL, "passed order is a Null Pointer");
	ELEKTRA_ASSERT (n > 0, "passed n <= 0");
	ELEKTRA_ASSERT (opmphmRatio >= 1, "opmphmRatio less 1");
	ELEKTRA_ASSERT (init->minOrder < init->maxOrder, "maxOrder - minOrder <= 0");
	// calculate result width
	opmphm->outputBase = opmphmGetWidth (init->maxOrder - init->minOrder);
	OpmphmOrder ** sortOrder = elektraMalloc (sizeof (OpmphmOrder *) * n);
	if (!sortOrder) return NULL;
	// transform desired hash map returning position
	for (size_t i = 0; i < n; ++i)
	{
		size_t result = order[i].index.p;
		size_t temp;
		for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
		{
			temp = result / opmphm->outputBase;
			order[i].index.t[t] = result - temp * opmphm->outputBase;
			result = temp;
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
 * @brief Allocates and initializes the opmphm.
 *
 * @retval Opmphm * success
 * @retval NULL memory error
 */
Opmphm * opmphmNew ()
{
	Opmphm * out = elektraMalloc (sizeof (Opmphm));
	if (!out)
	{
		return NULL;
	}
	for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
	{
		out->size[t] = 0;
	}
	return out;
}

/**
 * @brief Allocates and initializes if wanted the OpmphmOrder *.
 *
 * If init is true the OpmphmOrder * gets initialized with the default order of OpmphmInit->data.
 *
 * @param n the number of keys
 * @param init initialization or not
 *
 * @retval OpmphmOrder * success
 * @retval NULL memory error
 */
OpmphmOrder * opmphmNewOrder (size_t n, bool init)
{
	ELEKTRA_ASSERT (n > 0, "passed n <= 0");
	OpmphmOrder * out = elektraMalloc (sizeof (OpmphmOrder) * n);
	if (!out)
	{
		return NULL;
	}
	if (init)
	{
		for (size_t i = 0; i < n; ++i)
		{
			out[i].index.p = i;
		}
	}
	return out;
}

/**
 * @brief Deletes the Opmphm.
 *
 * Clears and frees all memory in Opmphm.
 *
 * @param opmphm the OPMPHM
 */
void opmphmDel (Opmphm * opmphm)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	opmphmClear (opmphm);
	elektraFree (opmphm);
}

/**
 * @brief Clears the Opmphm.
 *
 * Clears and frees all internal memory of Opmphm, but not the Opmphm.
 *
 * @param opmphm the OPMPHM
 */
void opmphmClear (Opmphm * opmphm)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	if (!opmphmIsEmpty (opmphm))
	{
		for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
		{
			elektraFree (opmphm->transsitions[t]);
			opmphm->size[t] = 0;
		}
	}
}

/**
 * @brief Determines if the Opmphm is Empty.
 *
 * @param opmphm the OPMPHM
 *
 * @retval true empty
 * @retval false non empty

 */
bool opmphmIsEmpty (Opmphm * opmphm)
{
	ELEKTRA_ASSERT (opmphm != NULL, "passed opmphm is a Null Pointer");
	for (unsigned int t = 0; t < OPMPHMTUPLE; ++t)
	{
		if (opmphm->size[t]) return false;
	}
	return true;
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

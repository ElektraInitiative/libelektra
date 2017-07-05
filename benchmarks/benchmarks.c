/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <benchmarks.h>
#include <sys/time.h>

struct timeval start;
int num_dir = NUM_DIR;
int num_key = NUM_KEY;
KeySet * large;

void timeInit (void)
{
	gettimeofday (&start, 0);
}

void timePrint (char * msg)
{
	struct timeval measure;
	time_t diff;

	gettimeofday (&measure, 0);

	diff = (measure.tv_sec - start.tv_sec) * 1000000 + (measure.tv_usec - start.tv_usec);
	fprintf (stdout, "%20s: %20d Microseconds\n", msg, (int)diff);

	gettimeofday (&start, 0);
}

void benchmarkCreate ()
{
	large = ksNew (num_key * num_dir, KS_END);
}

void benchmarkFillup ()
{
	int i, j;
	char name[KEY_NAME_LENGTH + 1];
	char value[] = "data";

	for (i = 0; i < num_dir; i++)
	{
		snprintf (name, KEY_NAME_LENGTH, "%s/%s%d", KEY_ROOT, "dir", i);
		ksAppendKey (large, keyNew (name, KEY_VALUE, value, KEY_END));
		for (j = 0; j < num_key; j++)
		{
			snprintf (name, KEY_NAME_LENGTH, "%s/%s%d/%s%d", KEY_ROOT, "dir", i, "key", j);
			ksAppendKey (large, keyNew (name, KEY_VALUE, value, KEY_END));
		}
	}
}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS

// needed for the opmphmRandom ()
#include "../src/libs/elektra/opmphm.c"

/**
 * @brief Fills a string up with random chars and null terminates it.
 *
 * @param name the string
 * @param length the length of the string (excluding the '\0')
 * @param seed to generate random data
 *
 */
static void fillUpWithRandomChars (char * name, int length, uint32_t * seed)
{
	int i = 0;
	char * c = name;
	while (i < length)
	{

		if ((opmphmRandom (seed) % 10) == 0)
		{
			/* SPECIAL CHARS:
			 *
			 * do not use '/' = 47 for generation
			 * 32-46 =  15
			 * 58-64 =   7
			 * 91-96 =   6
			 * 123-126 = 4
			 * ------------
			 *          32 chars
			 *
			 * skip the first 32
			 * add offset if not a special char
			 */
			*c = (opmphmRandom (seed) % 32) + 32;
			if (*c > 46)
			{
				*c += 11;
			}
			if (*c > 64)
			{
				*c += 26;
			}
			if (*c > 96)
			{
				*c += 26;
			}
			// not \ at end of keyname
			if (i == length - 1 && *c == 92)
			{
				continue;
			}
		}
		else
		{
			/* NON SPECIAL CHARS a-z A-Z 0-9
			 *
			 * 48-57 =  10
			 * 65-90 =  26
			 * 97-122 = 26
			 * ------------
			 *          62 chars
			 *
			 * skip the first 48
			 * add offset if special char
			 */
			*c = (opmphmRandom (seed) % 62) + 48;
			if (*c > 57)
			{
				*c += 7;
			}
			if (*c > 90)
			{
				*c += 6;
			}
		}
		++i;
		++c;
	}
	*c = '\0';
}

/**
 * @brief Generates a name for the Key and determines the number of sub Keys
 *
 * @param name the name so far
 * @param size the remaining size
 * @param out the KeySet to fill
 * @param seed seed for random actions
 * @param level the actual level
 * @param initSize the initial size
 * @param shape the shape of the KeySet
 *
 */
static void recGenerateKeySet (char * name, size_t * size, KeySet * out, uint32_t * seed, int level, size_t initSize, KeySetShape * shape)
{
	// to restore if key already in keyset
	size_t sizeBackup = *size;
	// generate name and add to path
	int length = (opmphmRandom (seed) % (shape->maxWordLength - shape->minWordLength)) + shape->minWordLength;
	char subName[strlen (name) + length + 2]; // 2 = '/' + '\0'
	strcpy (subName, name);
	subName[strlen (name)] = '/';
	fillUpWithRandomChars (&subName[strlen (name) + 1], length, seed);
	// determine subkeys
	size_t subKeys = shape->shapef (initSize, *size, level, seed);
	ELEKTRA_ASSERT (subKeys <= *size, "KsShapeFunction return value > size");
	// remove costs for subkeys
	if (subKeys)
	{
		*size -= (subKeys - 1); // the cost for one is included in the size from the parent call
		if (*size && (shape->parent == 1 || (opmphmRandom (seed) % shape->parent) == 0))
		{
			// can add because subKeys (no enforced add) and enough space
			if (ksLookupByName (out, subName, KDB_O_NONE))
			{
				// key is in out, therefore restore size and start from new
				*size = sizeBackup;
				recGenerateKeySet (name, size, out, seed, level, initSize, shape);
				return;
			}
			// counts extra so costs need to be removed
			--*size;
			if (ksAppendKey (out, keyNew (subName, KEY_END)) < 0)
			{
				fprintf (stderr, "generateKeySet: Can not add Key %s\n", subName);
				exit (EXIT_FAILURE);
			}
		}
	}
	else
	{
		// need to add because not subKeys
		if (ksLookupByName (out, subName, KDB_O_NONE))
		{
			// key is in out, therefore restore size and start from new
			recGenerateKeySet (name, size, out, seed, level, initSize, shape);
			return;
		}
		// no size decrement need because the costs where removed before
		if (ksAppendKey (out, keyNew (subName, KEY_END)) < 0)
		{
			fprintf (stderr, "generateKeySet: Can not add Key %s\n", subName);
			exit (EXIT_FAILURE);
		}
	}
	for (size_t i = 0; i < subKeys; ++i)
	{
		recGenerateKeySet (subName, size, out, seed, ++level, initSize, shape);
	}
}

/**
 * @brief Generates a KeySet for a given shape.
 *
 * The generateKeySet () is the start of the recursion and initialises the KeySet.
 *
 * @param size the desired KeySet size
 * @param seed the seed for the random generation
 * @param shape the KeySetShape
 *
 * @retval KeySet * the resulting KeySet
 */
KeySet * generateKeySet (size_t size, uint32_t * seed, KeySetShape * shape)
{
	ELEKTRA_ASSERT (size != 0, "size == 0");
	ELEKTRA_ASSERT (shape != NULL, "passed shape is NULL");
	ELEKTRA_ASSERT (shape->minWordLength < shape->maxWordLength, "minWordLength not < maxWordLength");
	ELEKTRA_ASSERT (shape->parent > 0, "parent <= 0");
	KeySet * out = ksNew (size, KS_END);
	size_t initSize = size;

	while (size)
	{
		char name[1];
		name[0] = '\0';
		--size;
		recGenerateKeySet (name, &size, out, seed, 1, initSize, shape);
	}
	return out;
}

#endif

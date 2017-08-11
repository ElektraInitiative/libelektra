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

void benchmarkCreate (void)
{
	large = ksNew (num_key * num_dir, KS_END);
}

void benchmarkFillup (void)
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

/**
 * Arbitrary Key Set Generator
 */

static size_t shapefDefault (size_t initSize ELEKTRA_UNUSED, size_t size, size_t level, int32_t * seed);

/**
 * @brief Fills a string up with random chars and null terminates it.
 *
 * @param name the string
 * @param length the length of the string (excluding the '\0')
 * @param seed to generate random data
 *
 */
const char * const alphabetnumbers = "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ0123456789";
const char * const alphabetspecial = "^!\"$%&/{([)]=} ?\\+*~#';,:.-_|<>";


static void fillUpWithRandomChars (char * name, size_t length, int32_t * seed, KeySetShape * shape)
{
	// trow the dice and use 8 bit of each byte from seed for one char, determination if special or not and which
	elektraRand (seed);
	size_t i = 0;
	size_t s = i;
	char * c = name;
	bool done = false;
	while (!done)
	{
		uint8_t ds = (*seed >> (8 * s * 2)) & 0xFF;
		uint8_t dc = (*seed >> (8 * (s * 2 + 1))) & 0xFF;
		// special or not
		bool special = shape->special && (ds % shape->special) == 0;
		// choose
		if (special)
		{
			*c = alphabetspecial[dc % strlen (alphabetspecial)];
		}
		else
		{
			*c = alphabetnumbers[dc % strlen (alphabetnumbers)];
		}
		++c;
		++i;
		++s;
		if (i == length)
		{
			done = true;
		}
		else if (s == 2)
		{
			elektraRand (seed);
			s = 0;
		}
	}
	*c = '\0';
}


/**
 * @brief Generates a name for the Key and determines the number of sub Keys
 *
 * @param key the actual key
 * @param size the remaining size
 * @param out the KeySet to fill
 * @param seed seed for random actions
 * @param level the actual level
 * @param initSize the initial size
 * @param shape the shape of the KeySet
 *
 */
static void recGenerateKeySet (Key * key, size_t * size, KeySet * out, int32_t * seed, size_t level, size_t initSize, KeySetShape * shape)
{
	// to restore if key already in keyset
	size_t sizeBackup = *size;
	Key * keyBackup = keyDup (key);
	if (!keyBackup)
	{
		fprintf (stderr, "generateKeySet: Can not dup Key %s\n", keyName (key));
		exit (EXIT_FAILURE);
	}
	elektraRand (seed);
	uint32_t dl = *seed & 0xFFFFFF;
	uint8_t dp = *seed >> 24;
	// generate name
	size_t length = (dl % (shape->maxWordLength - shape->minWordLength)) + shape->minWordLength;
	char subName[length + 1]; // + '\0'
	fillUpWithRandomChars (subName, length, seed, shape);
	if (keyAddBaseName (key, subName) < 0)
	{
		fprintf (stderr, "generateKeySet: Can not add KeyBaseName %s to key %s\n", subName, keyName (key));
		exit (EXIT_FAILURE);
	}
	// determine subkeys
	size_t subKeys = shape->shapef (initSize, *size, level, seed);
	ELEKTRA_ASSERT (subKeys <= *size + 1, "KsShapeFunction return value > size");
	// remove costs for subkeys
	if (subKeys)
	{
		*size -= (subKeys - 1); // the cost for one is included in the size from the parent call
		if (*size && shape->parent && (dp % shape->parent) == 0)
		{
			// counts extra so costs need to be removed
			--*size;
			ssize_t sizeBefore = ksGetSize (out);
			if (ksAppendKey (out, key) < 0)
			{
				fprintf (stderr, "generateKeySet: Can not add Key %s\n", subName);
				exit (EXIT_FAILURE);
			}
			if (sizeBefore == ksGetSize (out))
			{
				// key is in out, therefore restore size and start from new
				*size = sizeBackup;
				keyDel (key);
				recGenerateKeySet (keyBackup, size, out, seed, level, initSize, shape);
				return;
			}
		}
	}
	else
	{
		// no size decrement need because the costs where removed before
		ssize_t sizeBefore = ksGetSize (out);
		if (ksAppendKey (out, key) < 0)
		{
			fprintf (stderr, "generateKeySet: Can not add Key %s\n", subName);
			exit (EXIT_FAILURE);
		}
		if (sizeBefore == ksGetSize (out))
		{
			// key is in out, therefore restore size and start from new
			*size = sizeBackup;
			keyDel (key);
			recGenerateKeySet (keyBackup, size, out, seed, level, initSize, shape);
			return;
		}
	}
	keyDel (keyBackup);
	++level;
	for (size_t i = 0; i < subKeys; ++i)
	{
		Key * keydub = keyDup (key);
		if (!keydub)
		{
			fprintf (stderr, "generateKeySet: Can not dup Key %s\n", subName);
			exit (EXIT_FAILURE);
		}
		recGenerateKeySet (keydub, size, out, seed, level, initSize, shape);
	}
	keyDel (key);
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
KeySet * generateKeySet (size_t size, int32_t * seed, KeySetShape * shape)
{
	ELEKTRA_ASSERT (size != 0, "size == 0");
	KeySetShape shapeDefault;
	if (!shape)
	{
		/**
		 * Default KeySetShape
		 */
		shapeDefault.parent = 3;
		shapeDefault.special = 10;
		shapeDefault.minWordLength = 4;
		shapeDefault.maxWordLength = 7;
		shapeDefault.shapef = shapefDefault;

		shape = &shapeDefault;
	}
	ELEKTRA_ASSERT (shape->minWordLength < shape->maxWordLength, "minWordLength not < maxWordLength");
	ELEKTRA_ASSERT (shape->maxWordLength - shape->minWordLength <= 16777215, "max world length variation exceeded 16777215");
	ELEKTRA_ASSERT (shape->parent <= 127, "parent > 127");
	ELEKTRA_ASSERT (shape->special <= 127, "parent > 127");
	ELEKTRA_ASSERT (shape->minWordLength != 0, "minWordLength is 0");
	ELEKTRA_ASSERT (shape->maxWordLength != 0, "maxWordLength is 0");
	ELEKTRA_ASSERT (shape->shapef, "shape->shapef");
	KeySet * out = ksNew (size, KS_END);
	if (!out)
	{
		fprintf (stderr, "generateKeySet: Can not create KeySet\n");
		exit (EXIT_FAILURE);
	}
	size_t initSize = size;

	while (size)
	{
		--size;
		Key * k = keyNew ("", KEY_END);
		if (!k)
		{
			fprintf (stderr, "generateKeySet: Can not create root Key \n");
			exit (EXIT_FAILURE);
		}
		recGenerateKeySet (k, &size, out, seed, 1, initSize, shape);
	}
	return out;
}

/**
 * Default KeySetShape
 */

static size_t shapefDefault (size_t initSize ELEKTRA_UNUSED, size_t size, size_t level, int32_t * seed)
{
	if (!size) return 0;
	if (level > 5) return 0;
	if (size < level) return 1;
	elektraRand (seed);
	return *seed % (size / level);
}

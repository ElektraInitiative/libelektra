#include "benchmarks.h"

static uint32_t * getRandomSeed (uint32_t * seed);

static size_t shapef (size_t initSize ELEKTRA_UNUSED, size_t size, int level, uint32_t * seed ELEKTRA_UNUSED)
{
	if (!size) return 0;
	if (level > 3) return 0;
	return 1;
}

int main (int argc ELEKTRA_UNUSED, char ** argv ELEKTRA_UNUSED)
{
	uint32_t seed;
	if (getRandomSeed (&seed) != &seed)
	{
		fprintf (stderr, "FATAL: Seed Parsing Error or feed me more seeds\n");
		return EXIT_FAILURE;
	}
	KeySetShape shape;
	shape.parent = 1;
	shape.minWordLength = 3;
	shape.maxWordLength = 7;
	shape.shapef = shapef;

	KeySet * ks = generateKeySet (10, &seed, &shape);
	Key * key;
	ksRewind (ks);
	while ((key = ksNext (ks)))
	{
		printf ("%s\n", keyName (key));
	}
	ksDel (ks);

	return EXIT_SUCCESS;
}


static uint32_t * getRandomSeed (uint32_t * seed)
{
	// read from stdin
	char data[10 + 2]; // min = 0, max = 2^32 - 1, len(2^32 - 1) = 10 + '\n' + '\0'
	if (fgets (data, 12, stdin) != data)
	{
		return NULL;
	}
	// eliminate newline
	char * c;
	for (c = data; *c != '\n'; ++c)
		;
	*c = '\0';
	// convert to int
	char * pEnd;
	*seed = strtol (data, &pEnd, 10);
	if (*pEnd != '\0')
	{
		return NULL;
	}
	return seed;
}

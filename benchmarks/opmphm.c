/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map C benchmark.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include "benchmarks.h"

static int32_t * getRandomSeed (int32_t * seed);

int main (int argc ELEKTRA_UNUSED, char ** argv ELEKTRA_UNUSED)
{
	int32_t seed;
	if (getRandomSeed (&seed) != &seed)
	{
		fprintf (stderr, "FATAL: Seed Parsing Error or feed me more seeds\n");
		return EXIT_FAILURE;
	}

	KeySet * ks = generateKeySet (30, &seed, NULL);
	Key * key;
	ksRewind (ks);
	while ((key = ksNext (ks)))
	{
		printf ("%s\n", keyName (key));
	}
	ksDel (ks);

	return EXIT_SUCCESS;
}


static int32_t * getRandomSeed (int32_t * seed)
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

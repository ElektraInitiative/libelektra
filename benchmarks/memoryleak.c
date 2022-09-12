/**
 * @file
 *
 * @brief Benchmark for KDB
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdio.h>

#include <benchmarks.h>
#include <kdb.h>
#include <unistd.h>

#define NUM_RUNS 30

int main (void)
{
	ElektraKdb * handles[NUM_RUNS];
	ElektraKeyset * keysets[NUM_RUNS];

	ElektraKey * parentKey = keyNew ("user:/", ELEKTRA_KEY_END);

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		ElektraKdb * handle = kdbOpen (NULL, parentKey);

		ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

		kdbGet (handle, ks, parentKey);

		printf ("Retrieved %d keys\n", (int) ksGetSize (ks));

		handles[i] = handle;
		keysets[i] = ks;
	}

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		printf ("Freeing %d keys\n", (int) ksGetSize (keysets[i]));

		kdbClose (handles[i], parentKey);
		ksDel (keysets[i]);
	}

	keyDel (parentKey);
}

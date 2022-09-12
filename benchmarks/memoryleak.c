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

	ElektraKey * parentKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		ElektraKdb * handle = elektraKdbOpen (NULL, parentKey);

		ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

		elektraKdbGet (handle, ks, parentKey);

		printf ("Retrieved %d keys\n", (int) elektraKeysetGetSize (ks));

		handles[i] = handle;
		keysets[i] = ks;
	}

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		printf ("Freeing %d keys\n", (int) elektraKeysetGetSize (keysets[i]));

		elektraKdbClose (handles[i], parentKey);
		elektraKeysetDel (keysets[i]);
	}

	elektraKeyDel (parentKey);
}

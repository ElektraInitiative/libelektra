/**
 * @file
 *
 * @brief Benchmark for KDB
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdio.h>
#include <unistd.h>

#include "./benchmarks.h"

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>

#define NUM_RUNS 30

int main (void)
{
	KDB * handles[NUM_RUNS];
	KeySet * keysets[NUM_RUNS];

	Key * parentKey = keyNew ("user:/", KEY_END);

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		KDB * handle = elektraKdbOpen (NULL, parentKey);

		KeySet * ks = ksNew (0, KS_END);

		elektraKdbGet (handle, ks, parentKey);

		printf ("Retrieved %d keys\n", (int) ksGetSize (ks));

		handles[i] = handle;
		keysets[i] = ks;
	}

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		printf ("Freeing %d keys\n", (int) ksGetSize (keysets[i]));

		elektraKdbClose (handles[i], parentKey);
		ksDel (keysets[i]);
	}

	keyDel (parentKey);
}

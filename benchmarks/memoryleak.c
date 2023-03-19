/**
 * @file
 *
 * @brief Benchmark for KDB
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdio.h>

#include <benchmarks.h>
#include <elektra/old_kdb.h>
#include <unistd.h>

#define NUM_RUNS 30

int main (void)
{
	KDB * handles[NUM_RUNS];
	KeySet * keysets[NUM_RUNS];

	Key * parentKey = keyNew ("user:/", KEY_END);

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		KDB * handle = kdbOpen (NULL, parentKey);

		KeySet * ks = ksNew (0, KS_END);

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

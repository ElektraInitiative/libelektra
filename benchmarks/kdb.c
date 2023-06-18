/**
 * @file
 *
 * @brief Benchmark for KDB
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdio.h>

#include "./benchmarks.h"

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>

#define NUM_RUNS 7

#define CSV_STR_FMT "%s;%s;%d\n"


static void benchmarkDel (void)
{
	ksDel (large);
}

int main (void)
{
	benchmarkCreate ();
	benchmarkFillup ();

	fprintf (stdout, "%s;%s;%s\n", "plugin", "operation", "microseconds");
	{
		KeySet * returned = ksNew (0, KS_END);
		Key * parentKey = keyNew ("user:/", KEY_END);

		timeInit ();
		KDB * handle = elektraKdbOpen (NULL, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbOpen", timeGetDiffMicroseconds ());

		elektraKdbGet (handle, returned, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbGet", timeGetDiffMicroseconds ());

		// ksAppend (returned, large);
		elektraKdbSet (handle, large, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbSet", timeGetDiffMicroseconds ());
		elektraKdbClose (handle, parentKey);
		keyDel (parentKey);
		ksDel (returned);
	}

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		timeInit ();
		Key * parentKey = keyNew ("user:/benchmark", KEY_END);
		KDB * handle = elektraKdbOpen (NULL, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbOpen", timeGetDiffMicroseconds ());

		KeySet * returned = ksNew (0, KS_END);
		timeInit ();
		elektraKdbGet (handle, returned, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbGet", timeGetDiffMicroseconds ());

		elektraKdbClose (handle, parentKey);
		ksDel (returned);
		keyDel (parentKey);
	}

	benchmarkDel ();
}

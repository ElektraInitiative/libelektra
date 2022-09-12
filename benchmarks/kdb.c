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

#define NUM_RUNS 7

#define CSV_STR_FMT "%s;%s;%d\n"


static void benchmarkDel (void)
{
	elektraKeysetDel (large);
}

int main (void)
{
	benchmarkCreate ();
	benchmarkFillup ();

	fprintf (stdout, "%s;%s;%s\n", "plugin", "operation", "microseconds");
	{
		ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);
		ElektraKey * parentKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);

		timeInit ();
		ElektraKdb * handle = elektraKdbOpen (NULL, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbOpen", timeGetDiffMicroseconds ());

		elektraKdbGet (handle, returned, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbGet", timeGetDiffMicroseconds ());

		// ksAppend (returned, large);
		elektraKdbSet (handle, large, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbSet", timeGetDiffMicroseconds ());
		elektraKdbClose (handle, parentKey);
		elektraKeyDel (parentKey);
		elektraKeysetDel (returned);
	}

	for (size_t i = 0; i < NUM_RUNS; ++i)
	{
		timeInit ();
		ElektraKey * parentKey = elektraKeyNew ("user:/benchmark", ELEKTRA_KEY_END);
		ElektraKdb * handle = elektraKdbOpen (NULL, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbOpen", timeGetDiffMicroseconds ());

		ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);
		timeInit ();
		elektraKdbGet (handle, returned, parentKey);
		fprintf (stdout, CSV_STR_FMT, "core", "kdbGet", timeGetDiffMicroseconds ());

		elektraKdbClose (handle, parentKey);
		elektraKeysetDel (returned);
		elektraKeyDel (parentKey);
	}

	benchmarkDel ();
}

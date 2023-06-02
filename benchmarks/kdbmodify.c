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

#define CSV_STR_FMT "%s;%s;%d\n"


static void benchmarkDel (void)
{
	ksDel (large);
}

int main (void)
{
	fprintf (stdout, "%s;%s;%s\n", "plugin", "operation", "microseconds");
	timeInit ();

	Key * parentKey = keyNew ("user:/benchmark", KEY_END);
	KDB * handle = kdbOpen (NULL, parentKey);
	fprintf (stdout, CSV_STR_FMT, "core", "kdbOpen", timeGetDiffMicroseconds ());

	KeySet * returned = ksNew (0, KS_END);
	timeInit ();
	kdbGet (handle, returned, parentKey);
	fprintf (stdout, CSV_STR_FMT, "core", "kdbGet", timeGetDiffMicroseconds ());

	if (ksGetSize (returned) == 0)
	{
		fprintf (stderr, "error: no keys returned. make sure you actually have something in %s!", keyName (parentKey));
		goto error;
	}

	for (elektraCursor i = 0; i < ksGetSize (returned); i++)
	{
		// Modify every tenth key value
		if (i % 10 == 0)
		{
			Key * key = ksAtCursor (returned, i);
			keySetString (key, "modified value!");
		}
	}

error:
	kdbClose (handle, parentKey);
	ksDel (returned);
	keyDel (parentKey);

	benchmarkDel ();
}

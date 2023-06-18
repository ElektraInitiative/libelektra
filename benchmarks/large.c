/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./benchmarks.h"

KDB * kdb;
Key * key;

void benchmarkOpen (void)
{
	kdb = elektraKdbOpen (NULL, key);
}

void benchmarkInread (void)
{
	KeySet * n = ksNew (0, KS_END);
	elektraKdbGet (kdb, n, key);
	ksDel (n);
}

void benchmarkReadin (void)
{
	KeySet * n = ksNew (0, KS_END);
	elektraKdbGet (kdb, n, key);
	ksDel (n);
}

void benchmarkLookupByName (void)
{
	int i, j;
	char name[KEY_NAME_LENGTH + 1];

	for (i = 0; i < NUM_DIR; i++)
	{
		snprintf (name, KEY_NAME_LENGTH, "%s/%s%d", KEY_ROOT, "dir", i);
		ksLookupByName (large, name, 0);
		for (j = 0; j < NUM_KEY; j++)
		{
			snprintf (name, KEY_NAME_LENGTH, "%s/%s%d/%s%d", KEY_ROOT, "dir", i, "key", j);
			ksLookupByName (large, name, 0);
		}
	}
}

void benchmarkReread (void)
{
	elektraKdbGet (kdb, large, key);
}

void benchmarkInwrite (void)
{
	elektraKdbSet (kdb, large, key);
}

void benchmarkRewrite (void)
{
	elektraKdbSet (kdb, large, key);
}

void benchmarkWriteout (void)
{
	elektraKdbSet (kdb, large, key);
}

void benchmarkClose (void)
{
	elektraKdbClose (kdb, key);
}


int main (void)
{
	key = keyNew (KEY_ROOT, KEY_END);

	timeInit ();
	benchmarkCreate ();
	timePrint ("Created empty keyset");

	benchmarkFillup ();
	timePrint ("New large keyset");

	benchmarkOpen ();
	keySetName (key, KEY_ROOT);
	timePrint ("Opened key database");

	benchmarkInread ();
	timePrint ("Initialize read");

	benchmarkInwrite ();
	timePrint ("Initialize write");

	benchmarkWriteout ();
	timePrint ("Write key database");

	benchmarkRewrite ();
	timePrint ("Rewrite key database");

	benchmarkReadin ();
	timePrint ("Read in key database");

	benchmarkLookupByName ();
	timePrint ("Lookup key database");

	benchmarkReread ();
	timePrint ("Re read key database");

	benchmarkClose ();
	timePrint ("Closed key database");

	ksDel (large);
	keyDel (key);
}

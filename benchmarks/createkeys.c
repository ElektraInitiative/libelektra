/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./benchmarks.h"

void benchmarkDel (void)
{
	ksDel (large);
}

int benchmarkIterate (void)
{
	int c = 0;
	elektraCursor it;
	ssize_t ksSize = ksGetSize (large);

	for (it = 0; it < ksSize; ++it)
	{
		ksAtCursor (large, it);
		// count to make sure the loop is executed
		++c;
	}

	return c;
}

int main (int argc, char ** argv)
{
	if (argc != 3)
	{
		printf ("no arguments given, will exit\n");
		printf ("usage %s dir key (both dir+key are numbers)\n", argv[0]);
		exit (0);
	}
	else
	{
		num_dir = atoi (argv[1]);
		num_key = atoi (argv[2]);
		printf ("Using %d dirs %d keys\n", num_dir, num_key);
	}

	timeInit ();
	benchmarkCreate ();
	timePrint ("Created empty keyset");

	benchmarkFillup ();
	timePrint ("New large keyset");

	benchmarkIterate ();
	timePrint ("Iterated over keyset");

	benchmarkDel ();
	timePrint ("Del large keyset");
}

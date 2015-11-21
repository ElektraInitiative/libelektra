/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <benchmarks.h>

void benchmarkDel()
{
	ksDel (large);
}

int main(int argc, char** argv)
{
	if (argc != 3)
	{
		printf ("no arguments given, will exit\n");
		printf ("usage %s dir key (both dir+key are numbers)\n", argv[0]);
		exit(0);
	}
	else
	{
		num_dir = atoi(argv[1]);
		num_key = atoi(argv[2]);
		printf ("Using %d dirs %d keys\n", num_dir, num_key);
	}

	timeInit ();
	benchmarkCreate();
	timePrint ("Created empty keyset");

	benchmarkFillup();
	timePrint ("New large keyset");

	benchmarkDel();
	timePrint ("Del large keyset");
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <benchmarks.h>

#include <sys/time.h>

struct timeval start;
int num_dir = NUM_DIR;
int num_key = NUM_KEY;
KeySet *large;

void timeInit(void)
{
	gettimeofday (&start,0);
}

void timePrint(char * msg)
{
	struct timeval measure;
	time_t diff;

	gettimeofday (&measure, 0);

	diff = (measure.tv_sec - start.tv_sec) * 1000000 + (measure.tv_usec - start.tv_usec);
	fprintf (stdout, "%20s: %20d Microseconds\n", msg, (int)diff);
	
	gettimeofday (&start,0);
}

void benchmarkCreate()
{
	large = ksNew(num_key*num_dir, KS_END);
}

void benchmarkFillup ()
{
	int i,j;
	char name [KEY_NAME_LENGTH + 1];
	char value [] = "data";

	for (i=0; i< num_dir; i++)
	{
		snprintf (name, KEY_NAME_LENGTH, "%s/%s%d", KEY_ROOT, "dir", i);
		ksAppendKey(large, keyNew (name, KEY_VALUE, value, KEY_END));
		for (j=0; j<num_key; j++)
		{
			snprintf (name, KEY_NAME_LENGTH, "%s/%s%d/%s%d", KEY_ROOT, "dir", i, "key", j);
			ksAppendKey(large, keyNew (name, KEY_VALUE, value, KEY_END));
		}
	}
}

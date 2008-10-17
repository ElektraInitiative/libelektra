#include <benchmarks.h>

int nbTest;
int nbError;
struct timeval start;

pthread_t preader[NUM_THREAD];
pthread_t pwriter [NUM_THREAD];
pthread_t premover [NUM_THREAD];

void init_time (void)
{
	gettimeofday (&start,0);
}

void print_time (char * msg)
{
	struct timeval measure;
	time_t diff;

	gettimeofday (&measure, 0);

	diff = (measure.tv_sec - start.tv_sec) * 1000000 + (measure.tv_usec - start.tv_usec);
	fprintf (stdout, "%20s: %20d Microseconds\n", msg, (int)diff);
	
	gettimeofday (&start,0);
}

KeySet * create_keyset ()
{
	KeySet * set = ksNew(0);
	ksAppendKey(set, keyNew (ROOT_KEY, KEY_END));
	ksAppendKey(set, keyNew (ROOT_KEY "/key0", KEY_END));
	ksAppendKey(set, keyNew (ROOT_KEY "/key1",KEY_END));
	ksAppendKey(set, keyNew (ROOT_KEY "/key2",KEY_END));
	return set;
}

void compare_keyset (KeySet *ks1, KeySet *ks2)
{
	keyswitch_t failed=0;
	Key * orig;
	Key * read;
	ksRewind (ks1);
	ksRewind (ks2);
	read = ksNext (ks1);
	orig = ksNext (ks2);
	while (read) {
		nbTest ++;
		failed=keyCompare (orig, read);
		if (failed == KEY_FLAG_SYNC)
		{
			// fprintf (stderr, "succeded for : %s\n", keyName (read));
			// return;
		} else {
			fprintf (stderr, "failed for : %s\n", keyName (read) );
			nbError ++;
		}
		if (failed & KEY_TYPE) 
			fprintf (stderr, "type differs: is %d, was %d\n",
				keyGetType(read),keyGetType(orig));
		if (failed & KEY_NAME)
			fprintf (stderr, "name differs: is \"%s\", was \"%s\"\n",
				keyName(read),keyName(orig));
		if (failed & KEY_VALUE)
			fprintf (stderr, "value differs: is \"%s\", was \"%s\"\n",
				(char *) keyValue(read), (char *) keyValue(orig));
		if (failed & KEY_OWNER)
			fprintf (stderr, "owner differs: is \"%s\", was \"%s\"\n",
				keyOwner(read),keyOwner(orig));
		if (failed & KEY_COMMENT)
			fprintf (stderr, "comment differs: is \"%s\", was \"%s\"\n",
				keyComment(read),keyComment(orig));
		if (failed & KEY_UID)
			fprintf (stderr, "uid differs: is \"%d\", was \"%d\"\n",
				keyGetUID(read),keyGetUID(orig));
		if (failed & KEY_GID)
			fprintf (stderr, "gid differs: is \"%d\", was \"%d\"\n",
				keyGetGID(read),keyGetGID(orig));
		if (failed & KEY_MODE)
			fprintf (stderr, "mode differs: is \"0%o\", was \"0%o\"\n",
				keyGetMode(read),keyGetMode(orig));

		read = ksNext (ks1);
		orig = ksNext (ks2);
	}
}

void statistics (void)
{
	if (nbError!= 0)
	{
		printf ("\n******************************\n");
		printf (" %d    tests total\n", nbTest);
		printf ("-%d    tests passed\n", nbTest-nbError);
		printf ("-----\n");
		printf (" %d    tests failed", nbError);
		printf ("  (%3.2f %%)", ((double)nbError/(double)nbTest)*100.0);
		printf ("\n******************************\n");
	}
}


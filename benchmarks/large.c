#include <benchmarks.h>

int creator (KeySet *large)
{
	int i,j;
	char name [KEY_NAME_LENGTH + 1];
	char value [] = "data";

	for (i=0; i< NUM_DIR; i++)
	{
		snprintf (name, KEY_NAME_LENGTH, "%s/%s%d", KEY_ROOT, "dir", i);
		ksAppendKey(large, keyNew (name, KEY_VALUE, value, KEY_END));
		for (j=0; j<NUM_KEY; j++)
		{
			snprintf (name, KEY_NAME_LENGTH, "%s/%s%d/%s%d", KEY_ROOT, "dir", i, "key", j);
			ksAppendKey(large, keyNew (name, KEY_VALUE, value, KEY_END));
		}
	}

	return 1;
}

int main()
{
	KeySet * large = ksNew(NUM_KEY*NUM_DIR, KS_END);

	init_time ();

	succeed_if (creator (large), "could not create large keyset");
	print_time ("New large keyset");

	Key *key = keyNew (KEY_ROOT, KEY_END);
	KDB *kdb = kdbOpen(key);
	keySetName (key, KEY_ROOT);
	print_time ("Opened key database");

	KeySet *n = ksNew(0);
	kdbGet(kdb, n, key);
	ksDel (n);
	print_time ("Read in key database");

	kdbSet(kdb, large, key);
	print_time ("Write out key database");

	kdbClose(kdb, key);
	print_time ("Closed key database");

	ksDel (large);
	keyDel (key);
	return nbError;
}


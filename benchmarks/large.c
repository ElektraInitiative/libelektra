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
}

int marker (KeySet *ks2)
{
	Key * k;

	ksRewind (ks2);
	while ((k = ksNext(ks2)) != 0) keyRemove (k);
	ksSort (ks2); ksRewind (ks2);

	return 1;
}

void doNothing (Key *k)
{}

int iterator (KeySet *ks2)
{
	Key * k;

	ksRewind (ks2);
	while ((k = ksNext(ks2)) != 0) doNothing (k);

	return 1;
}

int xiter (KeySet *ks2)
{
	Key * k;
	int i;

	for (i=0; i< 5000; i++)
	{
		ksRewind (ks2);
		while ((k = ksNext(ks2)) != 0) doNothing (k);
	}

	return 1;
}

int main()
{
	KDB * h = kdbOpen();
	KeySet * large = ksNew(NUM_KEY*NUM_DIR+1, KS_END);
	KeySet * ks1 = ksNew(NUM_KEY*NUM_DIR+1, KS_END);
	KeySet * ks2 = ksNew(NUM_KEY*NUM_DIR+1, KS_END);
	KeySet * conf= ksNew (0);
	Key * root = keyNew (KEY_ROOT, KEY_VALUE, "filesys", KEY_END);

	init_time ();


	// kdbMount (h, root, conf);

	succeed_if (creator (large), "could not create large keyset");
	print_time ("New large keyset");

	succeed_if (kdbSet (h, large, root, 0) >= 0, "could not set large keyset");
	print_time ("Set large keyset");

	succeed_if (kdbSet (h, large, root, 0) >= 0, "could not reset large keyset");
	print_time ("Reset large keyset");

	succeed_if (kdbGet (h, ks1, root, 0) >= 0, "could not get large keyset");
	print_time ("Get large keyset");
	
	succeed_if (kdbGet (h, ks2, root, 0) >= 0, "could not get large keyset");
	print_time ("Get large keyset");

	succeed_if (marker (ks2), "could not remove large keyset");
	print_time ("Mark large keyset");

	succeed_if (iterator (ks2), "could not iterate large keyset");
	print_time ("Iterate large keyset");

	succeed_if (xiter (ks2), "could not iterate large keyset");
	print_time ("XIter large keyset");

	succeed_if (kdbSet (h, ks2, root, 0) >= 0, "could not delete large keyset");
	print_time ("Removed large keyset");

	keyDel (root);
	ksDel (large);
	ksDel (ks1);
	ksDel (ks2);
	ksDel (conf);
	kdbClose (h);
	return nbError;
}


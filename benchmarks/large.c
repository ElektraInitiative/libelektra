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

void doSomething(Key *k)
{
}

int internal_iterator (KeySet *ks)
{
	Key * k;

	ksRewind (ks);
	while ((k = ksNext(ks)) != 0) doSomething (k);

	return 1;
}

int external_iterator (KeySet *ks)
{
	for (int i = 0; i<ksGetSize(ks); ++i) doSomething (ks->array[i]);
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

	succeed_if (internal_iterator (large), "could not iterate large keyset");
	print_time ("XIter large keyset");

	succeed_if (external_iterator (large), "could not iterate large keyset");
	print_time ("Iterate large keyset");

	keyDel (root);
	ksDel (large);
	ksDel (ks1);
	ksDel (ks2);
	ksDel (conf);
	kdbClose (h);
	return nbError;
}


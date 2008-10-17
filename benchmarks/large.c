#include <benchmarks.h>

int main()
{
	KDB * h = kdbOpen();
	KeySet * large = ksNew(NUM_KEY*NUM_DIR+1, KS_END);
	KeySet * ks1 = ksNew(NUM_KEY*NUM_DIR+1, KS_END);
	KeySet * ks2 = ksNew(NUM_KEY*NUM_DIR+1, KS_END);
	KeySet * conf= ksNew (0);
	Key * root = keyNew (KEY_ROOT, KEY_VALUE, "filesys", KEY_END);
	Key * k;
	char name [KEY_NAME_LENGTH + 1];
	char value [] = "data";
	int i,j;

	init_time ();

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

	// kdbMount (h, root, conf);

	print_time ("New large keyset");
	
	succeed_if (kdbSet (h, large, root, 0) >= 0, "could not set large keyset");
	print_time ("Set large keyset");

	succeed_if (kdbSet (h, large, root, 0) >= 0, "could not reset large keyset");
	print_time ("Reset large keyset");

	succeed_if (kdbGet (h, ks1, root, 0) >= 0, "could not get large keyset");
	print_time ("Get large keyset");
	
	succeed_if (kdbGet (h, ks2, root, 0) >= 0, "could not reset large keyset");
	print_time ("Reset large keyset");

	ksRewind (ks2);
	// output_keyset (ks2,0);
	while ((k = ksNext(ks2)) != 0) keyRemove (k);
	ksSort (ks2); ksRewind (ks2);
	// output_keyset (ks2,0);
	succeed_if (kdbSet (h, ks2, root, 0) >= 0, "could not delete large keyset");

	// kdbUnmount (h, root);

	print_time ("delete large keyset");

	keyDel (root);
	ksDel (large);
	ksDel (ks1);
	ksDel (ks2);
	ksDel (conf);
	kdbClose (h);
	return nbError;
}


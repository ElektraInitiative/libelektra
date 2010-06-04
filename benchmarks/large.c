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
	while ((k = ksNext(ks)) != 0)
		doSomething (k);

	return 1;
}

int external_iterator (KeySet *ks)
{
	for (ssize_t i = 0; i<ksGetSize(ks); ++i)
		doSomething (ks->array[i]);
	return 1;
}


int main()
{
	KeySet * large = ksNew(NUM_KEY*NUM_DIR, KS_END);

	init_time ();

	succeed_if (creator (large), "could not create large keyset");
	print_time ("New large keyset");

	succeed_if (external_iterator (large), "could not iterate large keyset");
	print_time ("External Iterator");

	succeed_if (internal_iterator (large), "could not iterate large keyset");
	print_time ("Internal Iterator");

	ksDel (large);
	return nbError;
}


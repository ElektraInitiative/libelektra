#include <benchmarks.h>
#include <getopt.h>
#include <kdbdiff.h>
#include <kdbprivate.h>

int g_count = 50000;
int g_iterations = 10;
bool g_verbose = false;

static void processCommandLineArguments (int argc, char ** argv)
{
	struct option long_options[] = { { "count", required_argument, 0, 'c' },
					 { "iterations", required_argument, 0, 'i' },
					 { "verbose", no_argument, 0, 'v' },
					 { 0, 0, 0, 0 } };

	while (1)
	{
		int option_index = 0;
		int c = getopt_long (argc, argv, "", long_options, &option_index);

		if (c == -1)
		{
			break;
		}

		switch (c)
		{
		case 'c':
			g_count = atoi (optarg);
			break;
		case 'v':
			g_verbose = true;
			break;
		case 'i':
			g_iterations = atoi (optarg);
			break;
		default:
			break;
		}
	}
}


ElektraDiff * createBaseDiff (size_t count, size_t * keyCount)
{
	Key * parentKey = keyNew ("user:/test", KEY_END);

	KeySet * added = ksNew (count, KS_END);
	KeySet * modifiedOld = ksNew (count, KS_END);
	KeySet * modifiedNew = ksNew (count, KS_END);
	KeySet * removed = ksNew (count, KS_END);

	for (size_t i = 0; i < count; i++)
	{
		char * name = elektraFormat ("user:/test/originally_added_%zu", i);
		char * value = elektraFormat ("original_value_%zu", i);
		ksAppendKey (added, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		name = elektraFormat ("user:/test/originally_modified_%zu", i);
		value = elektraFormat ("original_modified_old_value_%zu", i);
		ksAppendKey (modifiedOld, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		name = elektraFormat ("user:/test/originally_modified_%zu", i);
		value = elektraFormat ("original_modified_new_value_%zu", i);
		ksAppendKey (modifiedNew, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		name = elektraFormat ("user:/test/originally_removed_%zu", i);
		value = elektraFormat ("original_value_%zu", i);
		ksAppendKey (removed, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);
	}

	*keyCount = ksGetSize (added) + ksGetSize (removed) + ksGetSize (modifiedOld) + ksGetSize (modifiedNew);

	ElektraDiff * diff = elektraDiffNew (added, removed, modifiedOld, modifiedNew, parentKey);
	keyDel (parentKey);

	return diff;
}

ElektraDiff * createDiffToAppend (size_t count, size_t * keyCount)
{
	Key * parentKey = keyNew ("user:/test", KEY_END);

	KeySet * added = ksNew (count, KS_END);
	KeySet * modifiedOld = ksNew (count, KS_END);
	KeySet * modifiedNew = ksNew (count, KS_END);
	KeySet * removed = ksNew (count, KS_END);

	size_t quarter = count / 4;
	size_t half = count / 2;

	for (size_t i = 0; i < quarter; i++)
	{
		// Add new keys
		char * name = elektraFormat ("user:/test/newly_added_%zu", i);
		char * value = elektraFormat ("original_value_%zu", i);
		ksAppendKey (added, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		// Add previously removed keys with new value --> modified
		name = elektraFormat ("user:/test/originally_removed_%zu", i);
		value = elektraFormat ("new_value_%zu", i);
		ksAppendKey (added, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		// Modify previously added keys
		name = elektraFormat ("user:/test/originally_added_%zu", i);
		value = elektraFormat ("original_value_%zu", i);
		ksAppendKey (modifiedOld, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		name = elektraFormat ("user:/test/originally_added_%zu", i);
		value = elektraFormat ("newly_modified_new_value_%zu", i);
		ksAppendKey (modifiedNew, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		// Modify previously modified keys
		name = elektraFormat ("user:/test/originally_modified_%zu", i);
		value = elektraFormat ("original_modified_new_value_%zu", i);
		ksAppendKey (modifiedOld, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		name = elektraFormat ("user:/test/originally_modified_%zu", i);
		value = elektraFormat ("newly_modified_new_value_%zu", i);
		ksAppendKey (modifiedNew, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		// Modify new keys
		name = elektraFormat ("user:/test/newly_modified_%zu", i);
		value = elektraFormat ("original_modified_new_value_%zu", i);
		ksAppendKey (modifiedOld, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		name = elektraFormat ("user:/test/newly_modified_%zu", i);
		value = elektraFormat ("newly_modified_new_value_%zu", i);
		ksAppendKey (modifiedNew, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		// Remove new keys
		name = elektraFormat ("user:/test/newly_removed_%zu", i);
		value = elektraFormat ("original_value_%zu", i);
		ksAppendKey (removed, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);
	}

	for (size_t i = half; i < half + quarter; i++)
	{
		// Remove previously added --> unchanged
		char * name = elektraFormat ("user:/test/originally_added_%zu", i);
		char * value = elektraFormat ("original_value_%zu", i);
		ksAppendKey (removed, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		// Remove previously modified -> removed
		name = elektraFormat ("user:/test/originally_modified_%zu", i);
		value = elektraFormat ("original_modified_new_value_%zu", i);
		ksAppendKey (removed, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);
	}

	for (size_t i = half + quarter; i < count; i++)
	{
		// Modify previously modified key back -> unchanged
		char * name = elektraFormat ("user:/test/originally_modified_%zu", i);
		char * value = elektraFormat ("original_modified_new_value_%zu", i);
		ksAppendKey (modifiedOld, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);

		name = elektraFormat ("user:/test/originally_modified_%zu", i);
		value = elektraFormat ("original_modified_old_value_%zu", i);
		ksAppendKey (modifiedNew, keyNew (name, KEY_VALUE, value, KEY_END));
		elektraFree (name);
		elektraFree (value);
	}

	*keyCount = ksGetSize (added) + ksGetSize (removed) + ksGetSize (modifiedOld) + ksGetSize (modifiedNew);

	ElektraDiff * diff = elektraDiffNew (added, removed, modifiedOld, modifiedNew, parentKey);
	keyDel (parentKey);

	return diff;
}

int main (int argc, char ** argv)
{
	processCommandLineArguments (argc, argv);

	size_t diff1keys = 0;
	size_t diff2keys = 0;

	ElektraDiff * diff1 = createBaseDiff (g_count, &diff1keys);
	ElektraDiff * diff2 = createDiffToAppend (g_count, &diff2keys);
	Key * parentKey = keyNew ("user:/test", KEY_END);

	if (g_verbose)
	{
		printf ("Diff1 keys: %zu\n", diff1keys);
		printf ("Diff2 keys: %zu\n", diff2keys);
	}

	long sum = 0;

	for (size_t i = 0; i < g_iterations; i++)
	{
		ElektraDiff * target = elektraDiffDup (diff1);

		timeInit ();

		elektraDiffAppend (target, diff2, parentKey);

		int us = timeGetDiffMicroseconds ();
		sum += us;

		printf ("%d\n", us);

		elektraDiffDel (target);
	}

	if (g_verbose)
	{
		double avgus = (double) sum / g_iterations;
		double avgms = avgus / 1000;

		printf ("Average: %.2f us\n", avgus);
		printf ("Average: %.2f ms\n", avgms);
	}

	keyDel (parentKey);
	elektraDiffDel (diff1);
	elektraDiffDel (diff2);
}

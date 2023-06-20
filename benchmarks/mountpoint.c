/**
 * This benchmark can be used to test the performance of kdbGet() and kdbSet() for a specified parent key in the KDB.
 * The benchmarks store their keys under <MP>/benchmark e.g. user:/mymountpoint/benchmark
 *
 * This plugin can be used to measure and compare the performance of different backend- or storage-plugins
 * and configurations.
 * Just create mountpoints with the configurations you want to benchmark and then run this benchmark on the mountpoints.
 *
 * At default, just 3 keys are used, this is intended for testing if the benchmark is working correctly.
 * For real benchmarks, you can specify a much larger value using the arguemnt '-c <number of keys'
 *
 */

#include <benchmarks.h>
#include <getopt.h>
#include <kdb.h>

/* Default values for optional arguments */
elektraCursor numKeys = 3;
bool harmonizeKeys = false;
bool useSingleKeySets = false;
bool withMeta = false;
bool onlyMeta = false;
bool verbose = false;

/* Use 3-char strings for key- and metakey-names and 5-char strings for values and metavalues to get comparable results */
char * keyNameFormat = "key%zu";
char * keyValueFormat = "value%zu";
char * keyValueModifiedFormat = "abcde%zu";

char * metaNameFormat = "met%zu";
char * metaValueFormat = "metav%zu";
char * metaValueModifiedFormat = "memod%zu";


static void printHelp (void)
{
	puts ("Optional arguments:");
	puts ("-c or --key-count <number of keys>: Specify the number of keys to benchmark.");
	puts ("-H or --harmonize-names");
	puts ("-h or --help: display this help");
	puts ("-s or --single-keysets: Store each key in its own KeySet for Benchmark 1");
	puts ("-m or --with-meta: include benchmarks for metadata, for the most reliable results it is recommended to run the tests for "
	      "metadata separately (see -M)");
	puts ("-M or --only-meta: only run benchmarks for metadata");
	puts ("-v or --verbose: more detailed output");
	exit (EXIT_SUCCESS);
}

static bool processCommandLineArguments (int argc, char ** argv)
{
	struct option long_options[] = { { "key-count", required_argument, 0, 'c' },
					 { "harmonize-names", no_argument, 0, 'H' },
					 { "help", no_argument, 0, 'h' },
					 { "single-keysets", no_argument, 0, 's' },
					 { "with-meta", no_argument, 0, 'm' },
					 { "only-meta", no_argument, 0, 'M' },
					 { "verbose", no_argument, 0, 'v' },
					 { 0, 0, 0, 0 } };

	while (1)
	{
		int option_index = 0;
		int c = getopt_long (argc, argv, "hsmMvc:", long_options, &option_index);

		if (c == -1)
		{
			break;
		}

		switch (c)
		{
		case 'c': {
			char * eptr;
			numKeys = strtol (optarg, &eptr, 10);

			if (!(*optarg) || *optarg == '-' || *eptr)
			{
				fprintf (stderr, "The provided key-count (%s), was not a valid size_t value!\n", optarg);
				return false;
			}
			break;
		}
		case 'h':
			printHelp ();
			break;
		case 'H':
			harmonizeKeys = true;
			keyNameFormat = "key%08zu";
			keyValueFormat = "value%08zu";
			keyValueModifiedFormat = "abcde%08zu";
			metaNameFormat = "met%08zu";
			metaValueFormat = "metav%08zu";
			metaValueModifiedFormat = "memod%08zu";
			break;
		case 's':
			useSingleKeySets = true;
			break;
		case 'm':
			withMeta = true;
			break;
		case 'M':
			onlyMeta = true;
			break;
		case 'v':
			verbose = true;
			break;
		default:
			fprintf (stderr, "The specified argument %c is not valid!", c);
			printHelp ();
		}
	}

	return true;
}

/**
 * @brief Run benchmarks that test the performance of `kdbSet` and `kdbGet` for a specified path in the KDB
 *
 *  The path where the benchmark data is written to is based on the key-name of the parent-key argument.
 */
int main (int argc, char ** argv)
{

	if (argc < 2)
	{
		fprintf (stderr, "Usage: %s [options] <parentKey>\n\n", argv[0]);
		printHelp ();
		return EXIT_FAILURE;
	}

	if (!processCommandLineArguments (argc, argv))
	{
		return EXIT_FAILURE;
	}

	if (argc < optind)
	{
		fprintf (stderr, "Usage: %s [options] <parentKey>\n", argv[0]);
		return EXIT_FAILURE;
	}


	if (verbose)
	{
		printf ("Number of keys: %zu\n", numKeys);
		printf ("Harmonize key names: %s\n", harmonizeKeys ? "true" : "false");
		printf ("Use single KeySets: %s\n", useSingleKeySets ? "true" : "false");
		printf ("Include benchmarks for metadata: %s\n", withMeta ? "true" : "false");
		printf ("Only run benchmarks for metadata: %s\n", onlyMeta ? "true" : "false");
	}


	char keyNameBuf[2048];
	char keyValueBuf[2048];


	if (strlen (argv[optind]) >= sizeof (keyNameBuf))
	{
		fprintf (stderr, "The length of the name of the given parent key was too long.\n");
		return EXIT_FAILURE;
	}

	char * strEnd = stpcpy (keyNameBuf, argv[optind]);

	if (strEnd[-1] != '/')
	{
		strEnd[0] = '/';
		strEnd[1] = '\0';
		strEnd++;
	}

	Key * parentKey = keyNew (argv[optind], KEY_END);

	if (verbose)
	{
		printf ("Name of parent key: %s\n", keyName (parentKey));
	}

	KeySet * ksBench = ksNew (numKeys, KS_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	kdbGet (kdb, ksBench, parentKey);

	int insertingTime = -1;
	int readTime = -1;
	int modifyTime = -1;
	int deleteTime = -1;
	int insertingMetaTime = -1;
	int readMetaTime = -1;
	int modifyMetaTime = -1;
	int deleteMetaTime = -1;

	if (!onlyMeta)
	{
		if (useSingleKeySets)
		{
			printf ("Benchmark 1: Store %zu keys, each in its own KeySet, without metadata.\n", numKeys);

			/* Allocate KeySets */
			KeySet ** ksBenchSingle = elektraMalloc (numKeys * sizeof (KeySet *));
			ksBenchSingle[0] = ksDup (ksBench);


			for (elektraCursor it = 0; it < numKeys; it++)
			{
				snprintf (strEnd, 2048 - (strEnd - keyNameBuf), keyNameFormat, it);
				snprintf (keyValueBuf, 2048, keyValueFormat, it);

				/* Use the KeySet from the previous iteration and add one key to it */
				if (it > 0)
				{
					ksBenchSingle[it] = ksDup (ksBenchSingle[it - 1]);
				}

				ksAppendKey (ksBenchSingle[it], keyNew (keyNameBuf, KEY_VALUE, keyValueBuf, KEY_END));
			}

			timeInit ();
			for (elektraCursor it = 0; it < numKeys; it++)
			{
				kdbSet (kdb, ksBenchSingle[it], parentKey);
			}

			insertingTime = timeGetDiffMicroseconds ();

			for (elektraCursor it = 0; it < numKeys; it++)
			{
				ksDel (ksBenchSingle[it]);
			}
			elektraFree (ksBenchSingle);
		}
		else
		{
			printf ("Benchmark 1: Store %zu keys in one KeySet without metadata.\n", numKeys);
			for (elektraCursor it = 0; it < numKeys; it++)
			{
				snprintf (strEnd, 2048 - (strEnd - keyNameBuf), keyNameFormat, it);
				snprintf (keyValueBuf, 2048, keyValueFormat, it);
				ksAppendKey (ksBench, keyNew (keyNameBuf, KEY_VALUE, keyValueBuf, KEY_END));
			}
			timeInit ();
			kdbSet (kdb, ksBench, parentKey);
			insertingTime = timeGetDiffMicroseconds ();
		}

		printf ("Benchmark 2: Read %zu keys from data source into KeySet.\n", numKeys);

		/* Close and re-open the KDB the get all data for the mountpoint */
		kdbClose (kdb, parentKey);
		kdb = kdbOpen (NULL, parentKey);
		ksClear (ksBench);

		timeInit ();
		kdbGet (kdb, ksBench, parentKey);
		readTime = timeGetDiffMicroseconds ();


		printf ("Benchmark 3: Modify %zu key-values in one KeySet without metadata.\n", numKeys);
		elektraCursor endPos;
		for (elektraCursor it = ksFindHierarchy (ksBench, parentKey, &endPos); it < endPos; it++)
		{
			snprintf (keyValueBuf, 2048, keyValueModifiedFormat, it);
			keySetString (ksAtCursor (ksBench, it), keyValueBuf);
		}
		timeInit ();
		kdbSet (kdb, ksBench, parentKey);
		modifyTime = timeGetDiffMicroseconds ();


		printf ("Benchmark 4: Delete %zu key-values in one KeySet without metadata.\n", numKeys);
		ksDel (ksCut (ksBench, parentKey));
		timeInit ();
		kdbSet (kdb, ksBench, parentKey);
		deleteTime = timeGetDiffMicroseconds ();
	}

	if (withMeta || onlyMeta)
	{
		printf ("Benchmark 5: Store one key with %zu metakeys.\n", numKeys);
		snprintf (strEnd, 2048 - (strEnd - keyNameBuf), "keyForMetaBenchmark");
		Key * benchMetaKey = keyNew (keyNameBuf, KEY_END);
		for (elektraCursor it = 0; it < numKeys; it++)
		{
			snprintf (keyNameBuf, 2048, metaNameFormat, it);
			snprintf (keyValueBuf, 2048, metaValueFormat, it);
			keySetMeta (benchMetaKey, keyNameBuf, keyValueBuf);
		}
		ksAppendKey (ksBench, keyDup (benchMetaKey, KEY_CP_ALL));
		timeInit ();
		kdbSet (kdb, ksBench, parentKey);
		insertingMetaTime = timeGetDiffMicroseconds ();


		printf ("Benchmark 6: Read one key with %zu metakeys from the data source.\n", numKeys);
		kdbClose (kdb, parentKey);
		kdb = kdbOpen (NULL, parentKey);
		ksClear (ksBench);

		timeInit ();
		kdbGet (kdb, ksBench, parentKey);
		readMetaTime = timeGetDiffMicroseconds ();


		printf ("Benchmark 7: Modify %zu values of metakeys.\n", numKeys);
		for (elektraCursor it = 0; it < numKeys; it++)
		{
			snprintf (keyNameBuf, 2048, metaNameFormat, it);
			snprintf (keyValueBuf, 2048, metaValueModifiedFormat, it);
			keySetMeta (benchMetaKey, keyNameBuf, keyValueBuf);
		}

		ksAppendKey (ksBench, keyDup (benchMetaKey, KEY_CP_ALL));
		timeInit ();
		kdbSet (kdb, ksBench, parentKey);
		modifyMetaTime = timeGetDiffMicroseconds ();

		printf ("Benchmark 8: Delete key with %zu metakeys.\n", numKeys);
		ksDel (ksCut (ksBench, parentKey));
		timeInit ();
		kdbSet (kdb, ksBench, parentKey);
		deleteMetaTime = timeGetDiffMicroseconds ();
	}

	kdbClose (kdb, parentKey);

	printf ("\n");

	if (!onlyMeta)
	{
		printf ("Insert Time (us); Read time (us); Modification Time (us); Deletion time (us)\n");
		printf ("%d; %d; %d; %d\n", insertingTime, readTime, modifyTime, deleteTime);
	}

	if (withMeta || onlyMeta)
	{
		printf ("Insert meta time (us); Read meta time; Modify meta time (us); Delete meta time (us)\n");
		printf ("%d; %d; %d; %d\n", insertingMetaTime, readMetaTime, modifyMetaTime, deleteMetaTime);
	}


	return EXIT_SUCCESS;
}

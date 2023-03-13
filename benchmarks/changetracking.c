#include <benchmarks.h>
#include <getopt.h>
#include <stdbool.h>

size_t keys = 50000;
char * keyNameFormat = "user:/test/key%zu";
char * keyValueFormat = "value%zu";
char * keyValueModifiedFormat = "value-modified%zu";

bool verbose = false;
bool harmonizeKeys = false;

void processCommandLineArguments (int argc, char ** argv)
{
	struct option long_options[] = { { "key-count", required_argument, 0, 'c' },
					 { "harmonize-names", no_argument, 0, 'h' },
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
			keys = atoi (optarg);
			break;
		case 'v':
			verbose = true;
			break;
		case 'h':
			harmonizeKeys = true;
			keyNameFormat = "user:/test/key%08zu";
			keyValueFormat = "value%08zu";
			keyValueModifiedFormat = "value-modified%08zu";
			break;
		default:
			break;
		}
	}
}

int main (int argc, char ** argv)
{
	processCommandLineArguments (argc, argv);

	if (verbose)
	{
		printf ("Number of keys: %zu\n", keys);
		printf ("Harmonize key names: %s\n", harmonizeKeys ? "true" : "false");
	}

	Key * parentKey = keyNew ("user:/test", KEY_END);
	KeySet * contract = ksNew (0, KS_END);
	KDB * kdb = kdbOpen (contract, parentKey);

	KeySet * ks = ksNew (0, KS_END);
	kdbGet (kdb, ks, parentKey);

	char nameBuffer[1024] = "";
	char valueBuffer[1024] = "";

	for (size_t i = 0; i < keys; i++)
	{
		snprintf (nameBuffer, 1023, keyNameFormat, i);
		snprintf (valueBuffer, 1023, keyValueFormat, i);
		ksAppendKey (ks, keyNew (nameBuffer, KEY_VALUE, valueBuffer, KEY_END));
	}

	timeInit ();
	kdbSet (kdb, ks, parentKey);
	int insertingTime = timeGetDiffMicroseconds ();

	ksClear (ks);
	kdbGet (kdb, ks, parentKey);

	size_t modified = 0;
	for (size_t i = keys / 2; i < keys; i++)
	{
		snprintf (nameBuffer, 1023, keyNameFormat, i);
		snprintf (valueBuffer, 1023, keyValueModifiedFormat, i);
		ksAppendKey (ks, keyNew (nameBuffer, KEY_VALUE, valueBuffer, KEY_END));
		modified++;
	}

	timeInit ();
	kdbSet (kdb, ks, parentKey);
	int modifyTime = timeGetDiffMicroseconds ();

	if (verbose)
	{
		printf ("Modified keys: %zu\n", modified);
		printf ("\n");
		printf ("Insert Time (us); Modification Time (us)\n");
	}

	printf ("%d;%d\n", insertingTime, modifyTime);

	return 0;
}

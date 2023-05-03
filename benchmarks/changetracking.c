#include "./benchmarks.h"

#include <getopt.h>
#include <stdbool.h>

size_t keys = 5000;
char * keyNameFormat = "user:/test/sw/org/myapp/#0/current/section/subsection/key%zu";
char * keyValueFormat = "value%zu";
char * keyValueModifiedFormat = "value-modified%zu";

KeySet * (*keySetBuilder) (void) = NULL;

bool verbose = false;
bool harmonizeKeys = false;

/**
 * @brief Power function.
 *
 * @param p basis
 * @param q exponent
 *
 * @retval size_t p^q
 */
static size_t getPower (size_t p, size_t q)
{
	size_t result = 1;
	for (size_t t = 0; t < q; ++t)
	{
		result *= p;
	}
	return result;
}

/**
 * binary tree
 */
static void shapefBinaryBranch (const size_t initSize, size_t size ELEKTRA_UNUSED, size_t level, int32_t * seed ELEKTRA_UNUSED,
				KsShapeFunctionReturn * ret, void * data ELEKTRA_UNUSED)
{
	size_t subKeys = 2;
	ret->label = 0;
	if (getPower (subKeys, level) > initSize)
	{
		ret->subKeys = 0;
	}
	else
	{
		ret->subKeys = subKeys;
	}
}

static KeySet * buildBinaryTree (void)
{
	KeySetShape shape;
	shape.minWordLength = 1;
	shape.maxWordLength = 1;
	shape.special = 0;
	shape.parent = 7;
	shape.shapeInit = NULL;
	shape.shapef = shapefBinaryBranch;
	shape.shapeDel = NULL;

	int32_t seed = 0xBEEF;
	KeySet * generated = generateKeySet (keys, &seed, &shape);
	KeySet * ks = ksNew (ksGetSize (generated), KS_END);

	char buffer[2048] = "";
	for (elektraCursor i = 0; i < ksGetSize (generated); i++)
	{
		Key * k = keyDup (ksAtCursor (generated, i), KEY_CP_ALL);
		snprintf (buffer, 2047, "user:/test%s", keyName (k));
		keySetName (k, buffer);
		ksAppendKey (ks, k);
	}

	ksDel (generated);

	return ks;
}

static KeySet * buildLinearTree (void)
{
	KeySet * ks = ksNew (0, KS_END);

	char nameBuffer[1024] = "";
	char valueBuffer[1024] = "";

	for (size_t i = 0; i < keys; i++)
	{
		snprintf (nameBuffer, 1023, keyNameFormat, i);
		snprintf (valueBuffer, 1023, keyValueFormat, i);
		ksAppendKey (ks, keyNew (nameBuffer, KEY_VALUE, valueBuffer, KEY_END));
	}

	return ks;
}

static void processCommandLineArguments (int argc, char ** argv)
{
	struct option long_options[] = { { "key-count", required_argument, 0, 'c' },
					 { "harmonize-names", no_argument, 0, 'h' },
					 { "verbose", no_argument, 0, 'v' },
					 { "binary-tree", no_argument, 0, 'b' },
					 { 0, 0, 0, 0 } };

	keySetBuilder = buildLinearTree;

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
		case 'b':
			keySetBuilder = buildBinaryTree;
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

	KeySet * generated = keySetBuilder ();
	ksAppend (ks, generated);
	ksDel (generated);

	timeInit ();
	kdbSet (kdb, ks, parentKey);
	int insertingTime = timeGetDiffMicroseconds ();

	kdbClose (kdb, parentKey);
	ksDel (ks);

	ks = ksNew (0, KS_END);
	kdb = kdbOpen (contract, parentKey);
	kdbGet (kdb, ks, parentKey);

	size_t modified = 0;
	for (size_t i = keys / 2; i < keys; i++)
	{
		char valueBuffer[1024] = "";
		Key * key = ksAtCursor (ks, i);
		snprintf (valueBuffer, 1023, keyValueModifiedFormat, i);
		keySetString (key, valueBuffer);
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

	kdbClose (kdb, parentKey);
	ksDel (contract);
	ksDel (ks);
	keyDel (parentKey);

	return 0;
}

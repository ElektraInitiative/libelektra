#include "tests.h"
#include <kdberrors.h>
#include <kdbmeta.h>
#include <kdbprivate.h>
#include <stdio.h>

static void test_warning_array (void)
{
	printf ("Test warning array\n");
	Key * warningKey = keyNew ("user/test", KEY_END);
	for (int i = 0; i <= MAX_WARNING_NUMBER + 2; i++)
	{
		char str[5];
		sprintf (str, "%d", i);
		elektraTriggerWarnings (1, warningKey, str);
	}

	char * lastEntry = elektraFormat ("warnings/#%d/reason", MAX_WARNING_NUMBER);
	char * nonExistentEntry = elektraFormat ("warnings/#%d/reason", MAX_WARNING_NUMBER + 1);
	const Key * nonExistentArrayElement = keyGetMeta (warningKey, nonExistentEntry);
	succeed_if (nonExistentArrayElement == NULL, "Exceeded maximum number of entries in warning array");

	const Key * lastArrayElement = keyGetMeta (warningKey, lastEntry);
	const char * lastEntryString = keyString (lastArrayElement);
	char * expected = elektraFormat ("%d", MAX_WARNING_NUMBER + 2);
	succeed_if (strcmp (lastEntryString, expected) == 0, "Saved wrong entry in last warning array element");
	elektraFree (lastEntry);
	elektraFree (warningKey);
	elektraFree (expected);
	elektraFree (nonExistentEntry);
}

int main (int argc, char ** argv)
{
	printf ("WARNING ARRAY TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_warning_array ();

	printf ("\ntest_array RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

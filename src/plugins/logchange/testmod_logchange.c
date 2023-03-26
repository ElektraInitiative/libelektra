/**
 * @file
 *
 * @brief Tests for logchange plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>

#include "logchange.h"
#include <kdbchangetracking.h>

ChangeTrackingContext * changeTrackingContext = NULL;

const ChangeTrackingContext * elektraChangeTrackingGetContextFromPlugin (ELEKTRA_UNUSED Plugin * plugin)
{
	return changeTrackingContext;
}

#ifdef __GLIBC__

static void setChangeTrackingContextForTest (ChangeTrackingContext * context)
{
	if (changeTrackingContext != NULL)
	{
		elektraChangeTrackingContextDel (changeTrackingContext);
	}

	changeTrackingContext = context;
}

#endif

static void test_getWithLogGetSet (void)
{
#ifdef __GLIBC__
	printf ("Testing %s\n", __func__);

	// Arrange
	KeySet * conf = ksNew (1, keyNew ("system:/log/get", KEY_VALUE, "1", KEY_END), KS_END);

	PLUGIN_OPEN ("logchange");

	Key * parentKey = keyNew ("system:/test", KEY_END);
	KeySet * ksOriginal = ksNew (1, keyNew ("system:/test/modifyme", KEY_VALUE, "xyz", KEY_END), KS_END);

	const char * expectedOutput = "loading configuration: system:/test\n";

	char * buffer;
	size_t bufferSize = 0;
	FILE * outstream = open_memstream (&buffer, &bufferSize);

	FILE * stdoutold = stdout;
	stdout = outstream;

	// Act
	plugin->kdbGet (plugin, ksOriginal, parentKey);

	fflush (outstream);
	stdout = stdoutold;

	// Assert
	succeed_if_same_string (buffer, expectedOutput);

	fclose (outstream);
	free (buffer);

	PLUGIN_CLOSE ();

	keyDel (parentKey);
	ksDecRef (ksOriginal);
	ksDel (ksOriginal);
#else
	printf ("Test %s disabled due to not using GLIBC\n", __func__);
#endif
}


static void test_changeTracking (void)
{
#ifdef __GLIBC__
	printf ("Testing %s\n", __func__);

	// Arrange
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("logchange");

	Key * parentKey = keyNew ("system:/test", KEY_END);
	KeySet * ksOriginal = ksNew (1, keyNew ("system:/test/existing", KEY_VALUE, "abc", KEY_END),
				     keyNew ("system:/test/modifyme", KEY_VALUE, "xyz", KEY_END),
				     keyNew ("system:/test/willbegone", KEY_VALUE, "123", KEY_END),
				     keyNew ("system:/test/onlymeta", KEY_VALUE, "999", KEY_META, "meta:/test", "123", KEY_END), KS_END);

	KeySet * ksModified = ksDeepDup (ksOriginal);
	keyDel (ksLookupByName (ksModified, "system:/test/willbegone", KDB_O_POP));
	keySetString (ksLookupByName (ksModified, "system:/test/modifyme", 0), "modified");
	ksAppendKey (ksModified, keyNew ("system:/test/hello", KEY_VALUE, "WORLD", KEY_END));

	keySetMeta (ksLookupByName (ksModified, "system:/test/onlymeta", 0), "meta:/new", "hi");

	const char * expectedOutput =
		"added key: system:/test/hello\n"
		"changed key: system:/test/modifyme\n"
		"changed key: system:/test/onlymeta\n"
		"removed key: system:/test/willbegone\n";

	plugin->kdbGet (plugin, ksOriginal, parentKey);

	ksIncRef (ksOriginal);
	setChangeTrackingContextForTest (elektraChangeTrackingCreateContextForTesting (ksOriginal));

	char * buffer;
	size_t bufferSize = 0;
	FILE * outstream = open_memstream (&buffer, &bufferSize);

	FILE * stdoutold = stdout;
	stdout = outstream;

	// Act
	plugin->kdbCommit (plugin, ksModified, parentKey);

	fflush (outstream);
	stdout = stdoutold;

	// Assert
	succeed_if_same_string (buffer, expectedOutput);

	fclose (outstream);
	free (buffer);

	PLUGIN_CLOSE ();

	keyDel (parentKey);
	ksDecRef (ksOriginal);
	ksDel (ksOriginal);
	ksDel (ksModified);
#else
	printf ("Test %s disabled due to not using GLIBC\n", __func__);
#endif
}


int main (int argc, char ** argv)
{
	printf ("LOGCHANGE     TESTS\n");
	printf ("===================\n\n");

	init (argc, argv);

	test_getWithLogGetSet ();
	test_changeTracking ();

	print_result ("testmod_logchange");

	if (changeTrackingContext != NULL)
	{
		elektraChangeTrackingContextDel (changeTrackingContext);
	}

	return nbError;
}

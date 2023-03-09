/**
 * @file
 *
 * @brief Tests for logchange plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>

#include "syslog.h"
#include <kdbchangetracking.h>

ChangeTrackingContext * changeTrackingContext = NULL;

const ChangeTrackingContext * elektraChangeTrackingGetContextFromPlugin (ELEKTRA_UNUSED Plugin * plugin)
{
	return changeTrackingContext;
}

static void setChangeTrackingContextForTest (ChangeTrackingContext * context)
{
	if (changeTrackingContext != NULL)
	{
		elektraChangeTrackingContextDel (changeTrackingContext);
	}

	changeTrackingContext = context;
}

FILE * syslogFile = NULL;
size_t syslogBufferSize = 0;
char * syslogBuffer = NULL;

static void freeSyslogBuffer (void)
{
	if (syslogFile != NULL)
	{
		fclose (syslogFile);
		syslogFile = NULL;
	}

	if (syslogBuffer != NULL)
	{
		free (syslogBuffer);
		syslogBuffer = NULL;
		syslogBufferSize = 0;
	}
}

static void createSyslogBuffer (void)
{
	freeSyslogBuffer ();
	syslogFile = open_memstream (&syslogBuffer, &syslogBufferSize);
	exit_if_fail (syslogFile != NULL, "error creating syslog buffer");
}

void syslog (int pri ELEKTRA_UNUSED, const char * fmt, ...)
{
	va_list argptr;
	va_start (argptr, fmt);
	vfprintf (syslogFile, fmt, argptr);
	va_end (argptr);
	fputs ("\n", syslogFile);
}

static void test (void)
{
	printf ("Test %s\n", __func__);

	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("syslog");

	Key * parentKey = keyNew ("system:/test", KEY_END);
	KeySet * ksOriginal = ksNew (1, keyNew ("system:/test/existing", KEY_VALUE, "abc", KEY_END),
				     keyNew ("system:/test/modifyme", KEY_VALUE, "xyz", KEY_END),
				     keyNew ("system:/test/willbegone", KEY_VALUE, "123", KEY_END),
				     keyNew ("system:/test/onlymeta", KEY_VALUE, "999", KEY_META, "meta:/test", "123", KEY_END), KS_END);

	for (elektraCursor i = 0; i < ksGetSize (ksOriginal); i++)
	{
		keyClearSync (ksAtCursor (ksOriginal, i));
	}

	KeySet * ksModified = ksDeepDup (ksOriginal);
	keyDel (ksLookupByName (ksModified, "system:/test/willbegone", KDB_O_POP));
	keySetString (ksLookupByName (ksModified, "system:/test/modifyme", 0), "modified");
	ksAppendKey (ksModified, keyNew ("system:/test/hello", KEY_VALUE, "WORLD", KEY_END));

	keySetMeta (ksLookupByName (ksModified, "system:/test/onlymeta", 0), "meta:/new", "hi");

	plugin->kdbGet (plugin, ksOriginal, parentKey);

	ksIncRef (ksOriginal);
	setChangeTrackingContextForTest (elektraChangeTrackingCreateContextForTesting (ksOriginal));

	createSyslogBuffer ();

	// Act
	plugin->kdbCommit (plugin, ksModified, parentKey);


	// Assert

	fflush (syslogFile);

	const char * expected =
		"change system:/test/hello to WORLD\n"
		"change system:/test/modifyme to modified\n"
		"change system:/test/onlymeta to 999\n"
		"committed configuration system:/test with 4 keys (3 changed)\n";

	succeed_if_same_string (syslogBuffer, expected);

	PLUGIN_CLOSE ();

	keyDel (parentKey);
	ksDecRef (ksOriginal);
	ksDel (ksOriginal);
	ksDel (ksModified);
}

int main (int argc, char ** argv)
{
	printf ("SYSLOG     TESTS\n");
	printf ("================\n\n");

	init (argc, argv);

	test ();

	print_result ("testmod_logchange");

	if (changeTrackingContext != NULL)
	{
		elektraChangeTrackingContextDel (changeTrackingContext);
	}

	freeSyslogBuffer ();

	return nbError;
}

/**
 * @file
 *
 * @brief Tests for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <elektra/kdb.h>
#include <elektra/opts.h>
#include <elektra/plugin/plugin.h>

#include <tests_plugin.h>

#include "testdata.h"

extern char ** environ;

static KeySet * getSpec (const char * name, Key ** parentKey)
{
	*parentKey = keyNew ("spec:/tests/gopts", KEY_END);

	if (strcmp (name, TEST_EMPTY) == 0)
	{
		return TEST_KS_EMPTY;
	}

	if (strcmp (name, TEST_SINGLEOPT) == 0)
	{
		return TEST_KS_SINGLEOPT;
	}

	if (strcmp (name, TEST_TWOOPT) == 0)
	{
		return TEST_KS_TWOOPT;
	}

	if (strcmp (name, TEST_SINGLEENV) == 0)
	{
		return TEST_KS_SINGLEENV;
	}

	if (strcmp (name, TEST_TWOENV) == 0)
	{
		return TEST_KS_TWOENV;
	}

	if (strcmp (name, TEST_MIXED) == 0)
	{
		return TEST_KS_MIXED;
	}

	yield_error ("unknown spec name");
	printf ("specname: %s\n", name);
	exit (EXIT_FAILURE);
}

int main (int argc, const char ** argv)
{
	const char * specname = argv[1];
	const char * appname = argv[0];
	argv[1] = appname;

	Key * parentKey;
	KeySet * ks = getSpec (specname, &parentKey);

	bool libFailed = elektraGetOpts (ks, argc - 1, &argv[1], (const char **) environ, parentKey) != 0;
	Key * libHelpKey = keyNew ("proc:/elektra/gopts/help", KEY_VALUE, "0", KEY_END);
	keyCopyAllMeta (libHelpKey, parentKey);
	ksAppendKey (ks, libHelpKey);

	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("gopts");

	Key * parentKey2;
	KeySet * ks2 = getSpec (specname, &parentKey2);

	bool pluginFailed = plugin->kdbGet (plugin, ks2, parentKey2) == ELEKTRA_PLUGIN_STATUS_ERROR;

	if (pluginFailed != libFailed)
	{
		PLUGIN_CLOSE ();
		ksDel (ks);
		keyDel (parentKey);
		ksDel (ks2);
		keyDel (parentKey2);
		char buf[256];
		strcpy (buf, "elektraGetOpts (");
		strcat (buf, libFailed ? "FAIL" : "OK");
		strcat (buf, ") differs from plugin->get (");
		strcat (buf, pluginFailed ? "FAIL" : "OK");
		strcat (buf, "): ");
		strncat (buf, specname, 128);
		yield_error (buf);

		return nbError;
	}

	compare_key (parentKey, parentKey2);
	compare_keyset (ks, ks2);

	PLUGIN_CLOSE ();

	ksDel (ks);
	keyDel (parentKey);
	ksDel (ks2);
	keyDel (parentKey2);

	return nbError;
}

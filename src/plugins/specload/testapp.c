/**
 * @file
 *
 * @brief Tests for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <elektra/kdbinvoke.h>
#include <elektra/kdbmodule.h>

#include "testdata.h"

// keep #ifdef in sync with kdb export
#ifdef _WIN32
#define STDOUT_FILENAME ("CON")
#else
#define STDOUT_FILENAME ("/dev/stdout")
#endif

static int outputKeySet (KeySet * ks, int noparent)
{
	Key * parentKey = keyNew (PARENT_KEY, KEY_END);

	if (noparent)
	{
		keySetMeta (parentKey, "system:/elektra/quickdump/noparent", "");
	}

	KeySet * specloadConf = ksNew (1, keyNew ("system:/sendspec", KEY_END), KS_END);
	ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, parentKey);

	int result = elektraInvoke2Args (specload, "sendspec", ks, parentKey);

	elektraInvokeClose (specload, parentKey);
	keyDel (parentKey);
	ksDel (specloadConf);

	return result == ELEKTRA_PLUGIN_STATUS_SUCCESS ? EXIT_SUCCESS : EXIT_FAILURE;
}

static int outputDefaultSpec (void)
{
	KeySet * ks = DEFAULT_SPEC;
	int result = outputKeySet (ks, 0);
	ksDel (ks);
	return result;
}

static int outputNoParentSpec (void)
{
	KeySet * ks = NOPARENT_SPEC;
	int result = outputKeySet (ks, 1);
	ksDel (ks);
	return result;
}

static int outputSpec (const char * name)
{
	if (strcmp (name, "default") == 0)
	{
		return outputDefaultSpec ();
	}

	if (strcmp (name, "noparent") == 0)
	{
		return outputNoParentSpec ();
	}

	return EXIT_FAILURE;
}

int main (int argc, const char ** argv)
{
	if (argc != 2 && argc != 3)
	{
		return EXIT_FAILURE;
	}

	if (strcmp (argv[1], "--elektra-spec") == 0)
	{
		return outputDefaultSpec ();
	}
	else if (strcmp (argv[1], "spec") == 0 && argc == 3)
	{
		return outputSpec (argv[2]);
	}

	return EXIT_FAILURE;
}

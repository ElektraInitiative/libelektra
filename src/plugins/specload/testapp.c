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

#include <kdbinvoke.h>
#include <kdbmodule.h>

#include "testdata.h"

// keep #ifdef in sync with kdb export
#ifdef _WIN32
#define STDOUT_FILENAME ("CON")
#else
#define STDOUT_FILENAME ("/dev/stdout")
#endif

static int outputKeySet (KeySet * ks)
{
	Key * errorKey = keyNew (0, KEY_END);

	KeySet * specloadConf = ksNew (1, keyNew ("system/sendspec", KEY_END), KS_END);
	ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, errorKey);

	int result = elektraInvoke2Args (specload, "sendspec", ks, NULL);

	elektraInvokeClose (specload, errorKey);
	keyDel (errorKey);
	ksDel (specloadConf);

	return result == ELEKTRA_PLUGIN_STATUS_SUCCESS ? EXIT_SUCCESS : EXIT_FAILURE;
}

static int outputDefaultSpec (void)
{
	KeySet * ks = DEFAULT_SPEC;
	int result = outputKeySet (ks);
	ksDel (ks);
	return result;
}

static int outputSpec (const char * name)
{
	if (strcmp (name, "default") == 0)
	{
		return outputDefaultSpec ();
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

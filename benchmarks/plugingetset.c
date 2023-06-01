/**
 * @file
 *
 * @brief Benchmark for get and set of storage plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <internal/kdbprivate.h>
#include <internal/pluginload/module.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>
#include <internal/utility/format.h>
int main (int argc, char ** argv)
{
	if (argc < 4 || argc > 5 || (argc == 5 && elektraStrCmp (argv[4], "get") != 0))
	{
		fprintf (stderr, "Usage: %s <path> <parent> <plugin> [get]\n", argv[0]);
		return 1;
	}

	typedef enum
	{
		BOTH,
		GET
	} Direction;

	Direction direction = BOTH;
	if (argc == 5) direction = GET;

	const char * path = argv[1];
	const char * parent = argv[2];
	const char * pluginname = argv[3];

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraFormat ("%s/test.%s.in", path, pluginname);
	char * outfile = elektraFormat ("%s/test.%s.out", path, pluginname);

	{
		Key * getKey = keyNew (parent, KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		KeySet * modules = ksNew (0, KS_END);
		elektraModulesInit (modules, 0);
		Key * errorKey = keyNew ("/", KEY_END);
		Plugin * plugin = elektraPluginOpen (pluginname, modules, conf, errorKey);
		keyDel (errorKey);

		plugin->kdbGet (plugin, ks, getKey);

		keyDel (getKey);
		elektraPluginClose (plugin, 0);
		elektraModulesClose (modules, 0);
		ksDel (modules);
	}

	if (ksGetSize (ks) <= 0)
	{
		return 1;
	}

	if (direction == BOTH)
	{
		Key * setKey = keyNew (parent, KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		KeySet * modules = ksNew (0, KS_END);
		elektraModulesInit (modules, 0);
		Key * errorKey = keyNew ("/", KEY_END);
		Plugin * plugin = elektraPluginOpen (pluginname, modules, conf, errorKey);
		keyDel (errorKey);
		plugin->kdbSet (plugin, ks, setKey);

		keyDel (setKey);
		elektraPluginClose (plugin, 0);
		elektraModulesClose (modules, 0);
		ksDel (modules);
	}

	elektraFree (infile);
	elektraFree (outfile);

	ksDel (ks);
	return 0;
}

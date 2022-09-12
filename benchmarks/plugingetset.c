/**
 * @file
 *
 * @brief Benchmark for get and set of storage plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#include <kdb.h>
#include <kdbhelper.h>
#include <kdbmodule.h>
#include <kdbprivate.h>

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

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	char * infile = elektraFormat ("%s/test.%s.in", path, pluginname);
	char * outfile = elektraFormat ("%s/test.%s.out", path, pluginname);

	{
		ElektraKey * getKey = elektraKeyNew (parent, ELEKTRA_KEY_VALUE, infile, ELEKTRA_KEY_END);

		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
		elektraModulesInit (modules, 0);
		ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
		Plugin * plugin = elektraPluginOpen (pluginname, modules, conf, errorKey);
		elektraKeyDel (errorKey);

		plugin->kdbGet (plugin, ks, getKey);

		elektraKeyDel (getKey);
		elektraPluginClose (plugin, 0);
		elektraModulesClose (modules, 0);
		elektraKeysetDel (modules);
	}

	if (elektraKeysetGetSize (ks) <= 0)
	{
		return 1;
	}

	if (direction == BOTH)
	{
		ElektraKey * setKey = elektraKeyNew (parent, ELEKTRA_KEY_VALUE, outfile, ELEKTRA_KEY_END);

		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
		elektraModulesInit (modules, 0);
		ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
		Plugin * plugin = elektraPluginOpen (pluginname, modules, conf, errorKey);
		elektraKeyDel (errorKey);
		plugin->kdbSet (plugin, ks, setKey);

		elektraKeyDel (setKey);
		elektraPluginClose (plugin, 0);
		elektraModulesClose (modules, 0);
		elektraKeysetDel (modules);
	}

	elektraFree (infile);
	elektraFree (outfile);

	elektraKeysetDel (ks);
	return 0;
}

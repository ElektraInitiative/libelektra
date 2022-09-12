/**
 * @file
 *
 * @brief Benchmark for storage plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdio.h>
#include <stdlib.h>

#include <benchmarks.h>
#include <tests.h>

#define CSV_STR_FMT "%s;%s;%d\n"

#define NUM_PLUGINS 4
#define NUM_RUNS 7

ElektraKeyset * modules[NUM_PLUGINS];
Plugin * plugins[NUM_PLUGINS];
char * pluginNames[NUM_PLUGINS] = { "dump", "mmapstorage_crc", "mmapstorage", "quickdump" };

static void benchmarkDel (void)
{
	elektraKeysetDel (large);
}

static int benchmarkOpenPlugins (void)
{
	for (size_t i = 0; i < NUM_PLUGINS; ++i)
	{
		modules[i] = elektraKeysetNew (0, ELEKTRA_KS_END);
		elektraModulesInit (modules[i], 0);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
		Plugin * plugin = elektraPluginOpen (pluginNames[i], modules[i], conf, errorKey);

		const ElektraKey * metaWarnings = elektraKeyGetMeta (errorKey, "warnings");
		if (metaWarnings) printf ("There are warnings for plugin: %s\n", pluginNames[i]);
		const ElektraKey * metaError = elektraKeyGetMeta (errorKey, "error");
		if (metaError) printf ("There are errors for plugin: %s\n", pluginNames[i]);

		if (plugin == 0)
		{
			printf ("Could not open plugin: %s\n", pluginNames[i]);
			return -1;
		}

		plugins[i] = plugin;
		elektraKeyDel (errorKey);
	}
	return 0;
}

static void benchmarkIterate (ElektraKeyset * ks)
{
	elektraKeysetRewind (ks);
	ElektraKey * cur;
	while ((cur = elektraKeysetNext (ks)))
	{
		__asm__("");
	}
}

static int benchmarkIterateName (ElektraKeyset * ks)
{
	elektraKeysetRewind (ks);
	ElektraKey * cur;
	const char * test = "foo";
	int i = 0;
	while ((cur = elektraKeysetNext (ks)))
	{
		i = strncmp (test, elektraKeyName (cur), 3);
	}
	return i;
}

static int benchmarkIterateValue (ElektraKeyset * ks)
{
	elektraKeysetRewind (ks);
	ElektraKey * cur;
	const char * test = "bar";
	int i = 0;
	while ((cur = elektraKeysetNext (ks)))
	{
		i = strncmp (test, elektraKeyString (cur), 3);
	}
	return i;
}

int main (int argc, char ** argv)
{
	// open all storage plugins
	if (benchmarkOpenPlugins () == -1) return -1;
	benchmarkCreate ();
	benchmarkFillup ();

	fprintf (stdout, "%s;%s;%s\n", "plugin", "operation", "microseconds");
	for (size_t i = 0; i < NUM_PLUGINS; ++i)
	{
		init (argc, argv);

		Plugin * plugin = plugins[i];
		ElektraKey * parentKey = elektraKeyNew ("user:/benchmarks/storage", ELEKTRA_KEY_VALUE, tmpfilename, ELEKTRA_KEY_END);

		for (size_t run = 0; run < NUM_RUNS; ++run)
		{
			timeInit ();
			if (plugin->kdbSet (plugin, large, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error writing with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "write keyset", timeGetDiffMicroseconds ());

			ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);
			if (plugin->kdbGet (plugin, returned, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "read keyset", timeGetDiffMicroseconds ());
			benchmarkIterate (returned);
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "iterate keyset", timeGetDiffMicroseconds ());
			elektraKeysetDel (returned);
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "delete keyset", timeGetDiffMicroseconds ());

			ElektraKeyset * returned2 = elektraKeysetNew (0, ELEKTRA_KS_END);
			if (plugin->kdbGet (plugin, returned2, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "re-read keyset", timeGetDiffMicroseconds ());
			elektraKeysetDel (returned2);
			timeInit ();

			ElektraKeyset * returned3 = elektraKeysetNew (0, ELEKTRA_KS_END);
			if (plugin->kdbGet (plugin, returned3, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			timeInit ();
			benchmarkIterateName (returned3);
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "strcmp key name", timeGetDiffMicroseconds ());
			elektraKeysetDel (returned3);
			timeInit ();

			ElektraKeyset * returned4 = elektraKeysetNew (0, ELEKTRA_KS_END);
			if (plugin->kdbGet (plugin, returned4, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			timeInit ();
			benchmarkIterateValue (returned4);
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "strcmp key value", timeGetDiffMicroseconds ());
			elektraKeysetDel (returned4);
			timeInit ();
		}
		clean_temp_home ();
		elektraKeyDel (parentKey);
	}

	benchmarkDel ();
}

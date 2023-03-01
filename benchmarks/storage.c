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

KeySet * modules[NUM_PLUGINS];
Plugin * plugins[NUM_PLUGINS];
char * pluginNames[NUM_PLUGINS] = { "dump", "mmapstorage_crc", "mmapstorage", "quickdump" };

static void benchmarkDel (void)
{
	ksDel (large);
}

static int benchmarkOpenPlugins (void)
{
	for (size_t i = 0; i < NUM_PLUGINS; ++i)
	{
		modules[i] = ksNew (0, KS_END);
		elektraModulesInit (modules[i], 0);
		KeySet * conf = ksNew (0, KS_END);
		Key * errorKey = keyNew ("/", KEY_END);
		Plugin * plugin = elektraPluginOpen (pluginNames[i], modules[i], conf, errorKey);

		const Key * metaWarnings = keyGetMeta (errorKey, "warnings");
		if (metaWarnings) printf ("There are warnings for plugin: %s\n", pluginNames[i]);
		const Key * metaError = keyGetMeta (errorKey, "error");
		if (metaError) printf ("There are errors for plugin: %s\n", pluginNames[i]);

		if (plugin == 0)
		{
			printf ("Could not open plugin: %s\n", pluginNames[i]);
			return -1;
		}

		plugins[i] = plugin;
		keyDel (errorKey);
	}
	return 0;
}

static void benchmarkIterate (KeySet * ks)
{
	elektraCursor it;
	ssize_t ksSize = ksGetSize (ks);

	for (it = 0; it < ksSize; ++it)
	{
		ksAtCursor (ks, it);
		__asm__ ("");
	}
}

static int benchmarkIterateName (KeySet * ks)
{
	const char * test = "foo";
	int i = 0;
	Key * cur;
	elektraCursor it;
	ssize_t ksSize = ksGetSize (ks);

	for (it = 0; it < ksSize; ++it)
	{
		cur = ksAtCursor (ks, it);
		i = strncmp (test, keyName (cur), 3);
	}
	return i;
}

static int benchmarkIterateValue (KeySet * ks)
{
	const char * test = "bar";
	int i = 0;
	Key * cur;
	elektraCursor it;
	ssize_t ksSize = ksGetSize (ks);

	for (it = 0; it < ksSize; ++it)
	{
		cur = ksAtCursor (ks, it);
		i = strncmp (test, keyString (cur), 3);
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
		Key * parentKey = keyNew ("user:/benchmarks/storage", KEY_VALUE, tmpfilename, KEY_END);

		for (size_t run = 0; run < NUM_RUNS; ++run)
		{
			timeInit ();
			if (plugin->kdbSet (plugin, large, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error writing with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "write keyset", timeGetDiffMicroseconds ());

			KeySet * returned = ksNew (0, KS_END);
			if (plugin->kdbGet (plugin, returned, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "read keyset", timeGetDiffMicroseconds ());
			benchmarkIterate (returned);
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "iterate keyset", timeGetDiffMicroseconds ());
			ksDel (returned);
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "delete keyset", timeGetDiffMicroseconds ());

			KeySet * returned2 = ksNew (0, KS_END);
			if (plugin->kdbGet (plugin, returned2, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "re-read keyset", timeGetDiffMicroseconds ());
			ksDel (returned2);
			timeInit ();

			KeySet * returned3 = ksNew (0, KS_END);
			if (plugin->kdbGet (plugin, returned3, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			timeInit ();
			benchmarkIterateName (returned3);
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "strcmp key name", timeGetDiffMicroseconds ());
			ksDel (returned3);
			timeInit ();

			KeySet * returned4 = ksNew (0, KS_END);
			if (plugin->kdbGet (plugin, returned4, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			timeInit ();
			benchmarkIterateValue (returned4);
			fprintf (stdout, CSV_STR_FMT, pluginNames[i], "strcmp key value", timeGetDiffMicroseconds ());
			ksDel (returned4);
			timeInit ();
		}
		clean_temp_home ();
		keyDel (parentKey);
	}
	benchmarkDel ();
}

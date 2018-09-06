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

#define NUM_PLUGINS 3
#define NUM_RUNS 7

KeySet * modules[NUM_PLUGINS];
Plugin * plugins[NUM_PLUGINS];
char * pluginNames[] = { "dump", "mmapstorage_crc", "mmapstorage" };

char * tmpfilename;
char * tempHome;
int tempHomeLen;
char * tempHomeConf;

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
		Key * errorKey = keyNew ("", KEY_END);
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
	ksRewind (ks);
	Key * cur;
	while ((cur = ksNext (ks)))
	{
		__asm__("");
	}
}

static int benchmarkIterateName (KeySet * ks)
{
	ksRewind (ks);
	Key * cur;
	const char * test = "foo";
	int i = 0;
	while ((cur = ksNext (ks)))
	{
		i = strncmp (test, keyName (cur), 3);
	}
	return i;
}

static int benchmarkIterateValue (KeySet * ks)
{
	ksRewind (ks);
	Key * cur;
	const char * test = "bar";
	int i = 0;
	while ((cur = ksNext (ks)))
	{
		i = strncmp (test, keyString (cur), 3);
	}
	return i;
}

int main (int argc, char ** argv)
{
	// open all storage plugins
	timeInit ();
	if (benchmarkOpenPlugins () == -1) return -1;
	timePrint ("Open storage plugins");

	timeInit ();
	benchmarkCreate ();
	timePrint ("Created empty keyset");

	benchmarkFillup ();
	timePrint ("New large keyset");

	for (size_t i = 0; i < NUM_PLUGINS; ++i)
	{
		printf ("Writing with plugin %s\n", pluginNames[i]);
		init (argc, argv);

		Plugin * plugin = plugins[i];
		fprintf (stdout, "Tmp: %s\n", tmpfilename);
		Key * parentKey = keyNew ("user/benchmarks/storage", KEY_VALUE, tmpfilename, KEY_END);
		timePrint ("init");

		for (size_t run = 0; run < NUM_RUNS; ++run)
		{
			if (plugin->kdbSet (plugin, large, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error writing with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			timePrint ("Write keyset");

			KeySet * returned = ksNew (0, KS_END);
			if (plugin->kdbGet (plugin, returned, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			timePrint ("Read keyset");

			benchmarkIterate (returned);
			ksDel (returned);
			timePrint ("Iterate over KS");

			KeySet * returned2 = ksNew (0, KS_END);
			if (plugin->kdbGet (plugin, returned2, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			ksDel (returned2);
			timePrint ("Re-read keyset");

			KeySet * returned3 = ksNew (0, KS_END);
			if (plugin->kdbGet (plugin, returned3, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			timePrint ("Read keyset");
			benchmarkIterateName (returned3);
			ksDel (returned3);
			timePrint ("Strcmp key names");

			KeySet * returned4 = ksNew (0, KS_END);
			if (plugin->kdbGet (plugin, returned4, parentKey) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
			{
				printf ("Error reading with plugin: %s\n", pluginNames[i]);
				return -1;
			}
			timePrint ("Read keyset");
			benchmarkIterateValue (returned4);
			ksDel (returned4);
			timePrint ("Strcmp key values");
		}

		clean_temp_home ();
		keyDel (parentKey);
		timePrint ("cleanup");
	}

	benchmarkDel ();
	timePrint ("Del large keyset");
}

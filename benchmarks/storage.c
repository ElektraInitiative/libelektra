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

#define NUM_PLUGINS 2
#define NUM_RUNS 7

KeySet * modules[NUM_PLUGINS];
Plugin * plugins[NUM_PLUGINS];
char * pluginNames[] = { "mmapstorage", "dump" };

char * tmpfilename;
char * tempHome;
int tempHomeLen;
char * tempHomeConf;

void benchmarkDel (void)
{
	ksDel (large);
}

int benchmarkOpenPlugins (void)
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

static void benchmarkCreateTemp (void)
{
	char * tmpvar;
	int fd;

	tmpvar = getenv ("TMPDIR");
	if (!tmpvar)
	{
		tmpvar = "/tmp";
	}
	// check tempvar for trailing slash /
	if (strlen (tmpvar) > 2)
	{
		if (tmpvar[strlen (tmpvar) - 1] == '/')
		{
			tmpvar[strlen (tmpvar) - 1] = '\0';
		}
	}

	tempHomeLen = strlen (tmpvar) + 1 + 13 + 6 + 1;
	tempHome = elektraMalloc (tempHomeLen);
	tempHomeConf = elektraMalloc (tempHomeLen + strlen (KDB_DB_USER) + 2);
	// succeed_if (tempHome != 0, "elektraMalloc failed");
	snprintf (tempHome, tempHomeLen, "%s/elektra-test.XXXXXX", tmpvar);
	snprintf (tempHomeConf, tempHomeLen, "%s/elektra-test.XXXXXX/" KDB_DB_USER, tmpvar);
	mkdtemp (tempHome);
	// succeed_if (mkdtemp (tempHome) != 0, "mkdtemp failed");
	setenv ("HOME", tempHome, 1);

	// atexit (clean_temp_home);

	int tmpfilenameLen = tempHomeLen + 1 + 12 + 6 + 1;
	tmpfilename = elektraMalloc (tmpfilenameLen);
	// succeed_if (tmpfilenameLen != 0, "elektraMalloc failed");
	snprintf (tmpfilename, tmpfilenameLen, "%s/elektra-tmp.XXXXXX", tempHome);
	fd = mkstemp (tmpfilename);
	// succeed_if (fd != -1, "mkstemp failed");
	close (fd);
}

static void benchmarkCleanTemp (void)
{
	if (tmpfilename)
	{
		unlink (tmpfilename);
		elektraFree (tmpfilename);
		tmpfilename = NULL;
	}

	if (tempHomeConf)
	{
		rmdir (tempHomeConf);
		elektraFree (tempHomeConf);
		tempHomeConf = NULL;
	}

	if (tempHome)
	{
		rmdir (tempHome);
		elektraFree (tempHome);
		tempHome = NULL;
		tempHomeLen = 0;
	}
}

size_t benchmarkIterate (KeySet * ks)
{
	ksRewind (ks);
	Key * cur;
	size_t c = 0;
	while ((cur = ksNext (ks)))
	{
		// count to make sure the loop is executed
		++c;
	}
	return c;
}

int main (void)
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
		benchmarkCreateTemp ();
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
		}

		benchmarkCleanTemp ();
		timePrint ("cleanup");
	}

	benchmarkDel ();
	timePrint ("Del large keyset");
}

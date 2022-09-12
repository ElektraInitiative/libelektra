/**
 * @file
 *
 * @brief Tests for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "specload.h"

#include <config.c>

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>
#include <tests_plugin.h>

#include "testdata.h"

#include <unistd.h>

static FILE * backupFile (const char * filename)
{
	FILE * in = fopen (filename, "rb");
	FILE * out = tmpfile ();

	char c;
	while ((c = fgetc (in)) != EOF)
	{
		fputc (c, out);
	}

	fclose (in);

	return out;
}

static void restoreBackup (FILE * backup, const char * filename)
{
	rewind (backup);
	FILE * out = fopen (filename, "wb");

	char c;
	while ((c = fgetc (backup)) != EOF)
	{
		fputc (c, out);
	}

	fclose (out);
	fclose (backup);
}

static void test_basics (bool directFile)
{
	printf ("test basics %s\n", directFile ? "directFile" : "");

	ElektraKey * parentKey = elektraKeyNew (PARENT_KEY, ELEKTRA_KEY_VALUE, srcdir_file ("specload/basics.quickdump"), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (directFile)
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/file", ELEKTRA_KEY_VALUE, srcdir_file ("specload/spec.quickdump"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	else
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/app", ELEKTRA_KEY_VALUE, bindir_file (TESTAPP_NAME), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");

	PLUGIN_OPEN ("specload");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * defaultSpec = DEFAULT_SPEC;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	compare_keyset (defaultSpec, ks);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	compare_keyset (defaultSpec, ks);

	elektraKeysetDel (defaultSpec);

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_newfile (bool directFile)
{
	printf ("test newfile %s\n", directFile ? "directFile" : "");

	exit_if_fail (access (srcdir_file ("specload/new.quickdump"), F_OK) == -1, "srcdir_file specload/new.quickdump shouldn't exist");

	ElektraKey * parentKey = elektraKeyNew (PARENT_KEY, ELEKTRA_KEY_VALUE, srcdir_file ("specload/new.quickdump"), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (directFile)
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/file", ELEKTRA_KEY_VALUE, srcdir_file ("specload/spec.quickdump"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	else
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/app", ELEKTRA_KEY_VALUE, bindir_file (TESTAPP_NAME), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");

	PLUGIN_OPEN ("specload");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * defaultSpec = DEFAULT_SPEC;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	compare_keyset (defaultSpec, ks);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	compare_keyset (defaultSpec, ks);

	elektraKeysetDel (defaultSpec);

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();

	remove (srcdir_file ("specload/new.quickdump"));
}

static void test_add (bool directFile)
{
	printf ("test add %s\n", directFile ? "directFile" : "");

	ElektraKey * parentKey = elektraKeyNew (PARENT_KEY, ELEKTRA_KEY_VALUE, srcdir_file ("specload/add.quickdump"), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (directFile)
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/file", ELEKTRA_KEY_VALUE, srcdir_file ("specload/spec.quickdump"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	else
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/app", ELEKTRA_KEY_VALUE, bindir_file (TESTAPP_NAME), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, elektraKeyNew (PARENT_KEY "/newkey", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END));
	ElektraKeyset * orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "adding values should fail");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, elektraKeyNew (PARENT_KEY "/newkey", ELEKTRA_KEY_META, "description", "Lorem ipsum", ELEKTRA_KEY_END));
	orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "adding description should work");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, elektraKeyNew (PARENT_KEY "/newkey", ELEKTRA_KEY_META, "opt/help", "Lorem ipsum opt", ELEKTRA_KEY_END));
	orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "adding opt/help should work");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_edit (bool directFile)
{
	printf ("test edit %s\n", directFile ? "directFile" : "");

	ElektraKey * parentKey = elektraKeyNew (PARENT_KEY, ELEKTRA_KEY_VALUE, srcdir_file ("specload/edit.quickdump"), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (directFile)
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/file", ELEKTRA_KEY_VALUE, srcdir_file ("specload/spec.quickdump"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	else
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/app", ELEKTRA_KEY_VALUE, bindir_file (TESTAPP_NAME), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, elektraKeyNew (PARENT_KEY "/key", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_META, "description", "Lorem ipsum", ELEKTRA_KEY_META, "opt/help",
				 "Lorem ipsum opt", ELEKTRA_KEY_END));
	ElektraKeyset * orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "changing values should fail");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, elektraKeyNew (PARENT_KEY "/key", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "description", "Lorem ipsum edit", ELEKTRA_KEY_META, "opt/help",
				 "Lorem ipsum opt", ELEKTRA_KEY_END));
	orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "changing description should work");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, elektraKeyNew (PARENT_KEY "/key", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "description", "Lorem ipsum", ELEKTRA_KEY_META, "opt/help",
				 "Lorem ipsum opt edit", ELEKTRA_KEY_END));
	orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "changing opt/help should work");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_remove (bool directFile)
{
	printf ("test remove %s\n", directFile ? "directFile" : "");

	ElektraKey * parentKey = elektraKeyNew (PARENT_KEY, ELEKTRA_KEY_VALUE, srcdir_file ("specload/remove.quickdump"), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (directFile)
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/file", ELEKTRA_KEY_VALUE, srcdir_file ("specload/spec.quickdump"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	else
	{
		conf = elektraKeysetNew (2, elektraKeyNew ("/app", ELEKTRA_KEY_VALUE, bindir_file (TESTAPP_NAME), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks,
		     elektraKeyNew (PARENT_KEY "/key", ELEKTRA_KEY_META, "description", "Lorem ipsum", ELEKTRA_KEY_META, "opt/help", "Lorem ipsum opt", ELEKTRA_KEY_END));
	ElektraKeyset * orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "removing values should fail");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, elektraKeyNew (PARENT_KEY "/key", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "opt/help", "Lorem ipsum opt", ELEKTRA_KEY_END));
	orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "removing description should work");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, elektraKeyNew (PARENT_KEY "/key", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_META, "description", "Lorem ipsum", ELEKTRA_KEY_END));
	orig = elektraKeysetDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "removing opt/help should work");
	compare_keyset (orig, ks);
	elektraKeysetDel (ks);
	elektraKeysetDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("SPECLOAD     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics (true);
	test_basics (false);

	test_add (true);
	test_add (false);

	test_edit (true);
	test_edit (false);

	test_remove (true);
	test_remove (false);

	test_newfile (true);
	test_newfile (false);

	print_result ("testmod_specload");

	return nbError;
}

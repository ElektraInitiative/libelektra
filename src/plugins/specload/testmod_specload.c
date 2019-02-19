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

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("spec/tests/specload", KEY_VALUE, srcdir_file ("specload/basics.quickdump"), KEY_END);
	KeySet * conf = ksNew (2, keyNew ("/app", KEY_VALUE, testapp_path, KEY_END), KS_END);

	succeed_if (elektraSpecloadCheckConfig (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
		    "call to checkConfig was not successful");

	PLUGIN_OPEN ("specload");

	KeySet * ks = ksNew (0, KS_END);
	KeySet * defaultSpec = DEFAULT_SPEC;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	compare_keyset (defaultSpec, ks);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");
	compare_keyset (defaultSpec, ks);

	ksDel (defaultSpec);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_add (void)
{
	printf ("test add\n");

	Key * parentKey = keyNew ("spec/tests/specload", KEY_VALUE, srcdir_file ("specload/add.quickdump"), KEY_END);
	KeySet * conf = ksNew (2, keyNew ("/app", KEY_VALUE, testapp_path, KEY_END), KS_END);

	succeed_if (elektraSpecloadCheckConfig (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
		    "call to checkConfig was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/add.quickdump"));
	KeySet * ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/newkey", KEY_VALUE, "0", KEY_END));
	KeySet * orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "adding values should fail");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/newkey", KEY_META, "default", "0", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "adding default should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/newkey", KEY_META, "type", "string", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "adding type should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/newkey", KEY_META, "description", "Lorem ipsum", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "adding description should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/newkey", KEY_META, "opt/help", "Lorem ipsum opt", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "adding opt/help should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_edit (void)
{
	printf ("test edit\n");

	Key * parentKey = keyNew ("spec/tests/specload", KEY_VALUE, srcdir_file ("specload/edit.quickdump"), KEY_END);
	KeySet * conf = ksNew (2, keyNew ("/app", KEY_VALUE, testapp_path, KEY_END), KS_END);

	succeed_if (elektraSpecloadCheckConfig (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
		    "call to checkConfig was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	KeySet * ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/key", KEY_VALUE, "1", KEY_META, "default", "0", KEY_META, "description",
				 "Lorem ipsum", KEY_META, "opt/help", "Lorem ipsum opt", KEY_END));
	KeySet * orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "changing values should fail");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/key", KEY_VALUE, "0", KEY_META, "default", "1", KEY_META, "description",
				 "Lorem ipsum", KEY_META, "opt/help", "Lorem ipsum opt", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "changing default should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/key", KEY_VALUE, "0", KEY_META, "default", "0", KEY_META, "description",
				 "Lorem ipsum edit", KEY_META, "opt/help", "Lorem ipsum opt", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "changing description should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/key", KEY_VALUE, "0", KEY_META, "default", "0", KEY_META, "description",
				 "Lorem ipsum", KEY_META, "opt/help", "Lorem ipsum opt edit", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "changing opt/help should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_remove (void)
{
	printf ("test remove\n");

	Key * parentKey = keyNew ("spec/tests/specload", KEY_VALUE, srcdir_file ("specload/remove.quickdump"), KEY_END);
	KeySet * conf = ksNew (2, keyNew ("/app", KEY_VALUE, testapp_path, KEY_END), KS_END);

	succeed_if (elektraSpecloadCheckConfig (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
		    "call to checkConfig was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	KeySet * ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/key", KEY_META, "description", "Lorem ipsum", KEY_META, "opt/help", "Lorem ipsum opt",
				 KEY_END));
	KeySet * orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "removing values should fail");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/key", KEY_VALUE, "0", KEY_META, "opt/help", "Lorem ipsum opt", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "removing description should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("spec/tests/specload/key", KEY_VALUE, "0", KEY_META, "description", "Lorem ipsum", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "removing opt/help should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("SPECLOAD     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_add ();
	test_edit ();
	test_remove ();

	print_result ("testmod_specload");

	return nbError;
}

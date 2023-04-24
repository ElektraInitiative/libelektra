/**
 * @file
 *
 * @brief Tests for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./specload.h"

#include <config.c>

#include <stdlib.h>
#include <string.h>

#include <internal/kdb/config.h>
#include <tests_plugin.h>

#include "./testdata.h"

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

	Key * parentKey = keyNew (PARENT_KEY, KEY_VALUE, srcdir_file ("specload/basics.quickdump"), KEY_END);
	KeySet * conf;
	if (directFile)
	{
		conf = ksNew (2, keyNew ("/file", KEY_VALUE, srcdir_file ("specload/spec.quickdump"), KEY_END), KS_END);
	}
	else
	{
		conf = ksNew (2, keyNew ("/app", KEY_VALUE, bindir_file (TESTAPP_NAME), KEY_END), KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");

	PLUGIN_OPEN ("specload");

	KeySet * ks = ksNew (0, KS_END);
	KeySet * defaultSpec = DEFAULT_SPEC;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	compare_keyset (defaultSpec, ks);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	compare_keyset (defaultSpec, ks);

	ksDel (defaultSpec);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_newfile (bool directFile)
{
	printf ("test newfile %s\n", directFile ? "directFile" : "");

	exit_if_fail (access (srcdir_file ("specload/new.quickdump"), F_OK) == -1, "srcdir_file specload/new.quickdump shouldn't exist");

	Key * parentKey = keyNew (PARENT_KEY, KEY_VALUE, srcdir_file ("specload/new.quickdump"), KEY_END);
	KeySet * conf;
	if (directFile)
	{
		conf = ksNew (2, keyNew ("/file", KEY_VALUE, srcdir_file ("specload/spec.quickdump"), KEY_END), KS_END);
	}
	else
	{
		conf = ksNew (2, keyNew ("/app", KEY_VALUE, bindir_file (TESTAPP_NAME), KEY_END), KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");

	PLUGIN_OPEN ("specload");

	KeySet * ks = ksNew (0, KS_END);
	KeySet * defaultSpec = DEFAULT_SPEC;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	compare_keyset (defaultSpec, ks);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	compare_keyset (defaultSpec, ks);

	ksDel (defaultSpec);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();

	remove (srcdir_file ("specload/new.quickdump"));
}

static void test_add (bool directFile)
{
	printf ("test add %s\n", directFile ? "directFile" : "");

	Key * parentKey = keyNew (PARENT_KEY, KEY_VALUE, srcdir_file ("specload/add.quickdump"), KEY_END);
	KeySet * conf;
	if (directFile)
	{
		conf = ksNew (2, keyNew ("/file", KEY_VALUE, srcdir_file ("specload/spec.quickdump"), KEY_END), KS_END);
	}
	else
	{
		conf = ksNew (2, keyNew ("/app", KEY_VALUE, bindir_file (TESTAPP_NAME), KEY_END), KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/add.quickdump"));
	KeySet * ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/newkey", KEY_VALUE, "0", KEY_END));
	KeySet * orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "adding values should fail");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/newkey", KEY_META, "description", "Lorem ipsum", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "adding description should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	backup = backupFile (srcdir_file ("specload/add.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/newkey", KEY_META, "opt/help", "Lorem ipsum opt", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "adding opt/help should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/add.quickdump"));

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_edit (bool directFile)
{
	printf ("test edit %s\n", directFile ? "directFile" : "");

	Key * parentKey = keyNew (PARENT_KEY, KEY_VALUE, srcdir_file ("specload/edit.quickdump"), KEY_END);
	KeySet * conf;
	if (directFile)
	{
		conf = ksNew (2, keyNew ("/file", KEY_VALUE, srcdir_file ("specload/spec.quickdump"), KEY_END), KS_END);
	}
	else
	{
		conf = ksNew (2, keyNew ("/app", KEY_VALUE, bindir_file (TESTAPP_NAME), KEY_END), KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	KeySet * ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/key", KEY_VALUE, "1", KEY_META, "description", "Lorem ipsum", KEY_META, "opt/help",
				 "Lorem ipsum opt", KEY_END));
	KeySet * orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "changing values should fail");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/key", KEY_VALUE, "0", KEY_META, "description", "Lorem ipsum edit", KEY_META, "opt/help",
				 "Lorem ipsum opt", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "changing description should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	backup = backupFile (srcdir_file ("specload/edit.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/key", KEY_VALUE, "0", KEY_META, "description", "Lorem ipsum", KEY_META, "opt/help",
				 "Lorem ipsum opt edit", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "changing opt/help should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/edit.quickdump"));

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_remove (bool directFile)
{
	printf ("test remove %s\n", directFile ? "directFile" : "");

	Key * parentKey = keyNew (PARENT_KEY, KEY_VALUE, srcdir_file ("specload/remove.quickdump"), KEY_END);
	KeySet * conf;
	if (directFile)
	{
		conf = ksNew (2, keyNew ("/file", KEY_VALUE, srcdir_file ("specload/spec.quickdump"), KEY_END), KS_END);
	}
	else
	{
		conf = ksNew (2, keyNew ("/app", KEY_VALUE, bindir_file (TESTAPP_NAME), KEY_END), KS_END);
	}

	succeed_if (elektraSpecloadCheckConf (parentKey, conf) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to checkConf was not successful");


	PLUGIN_OPEN ("specload");

	FILE * backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	KeySet * ks = ksNew (0, KS_END);
	ksAppendKey (ks,
		     keyNew (PARENT_KEY "/key", KEY_META, "description", "Lorem ipsum", KEY_META, "opt/help", "Lorem ipsum opt", KEY_END));
	KeySet * orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "removing values should fail");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/key", KEY_VALUE, "0", KEY_META, "opt/help", "Lorem ipsum opt", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "removing description should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	backup = backupFile (srcdir_file ("specload/remove.quickdump"));
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/key", KEY_VALUE, "0", KEY_META, "description", "Lorem ipsum", KEY_END));
	orig = ksDup (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "removing opt/help should work");
	compare_keyset (orig, ks);
	ksDel (ks);
	ksDel (orig);
	restoreBackup (backup, srcdir_file ("specload/remove.quickdump"));

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_sendspec (void)
{
	printf ("test sendspec\n");

	KeySet * ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew (PARENT_KEY "/key", KEY_META, "description", "abc", KEY_META, "opt/help", "def", KEY_END));
	KeySet * conf = ksNew (2, keyNew ("/file", KEY_VALUE, srcdir_file ("specload/spec.quickdump"), KEY_END), KS_END);
	Key * parentKey = keyNew (PARENT_KEY, KEY_END);

	PLUGIN_OPEN ("specload");

	int tmp = elektraSpecloadSendSpec (plugin, NULL, parentKey);
	succeed_if (tmp == ELEKTRA_PLUGIN_STATUS_ERROR, "sendspec should return an error if spec is NULL");

	tmp = elektraSpecloadSendSpec (plugin, ks, NULL);
	succeed_if (tmp == ELEKTRA_PLUGIN_STATUS_ERROR, "sendspec should return an error if parentKey is NULL");

	tmp = elektraSpecloadSendSpec (plugin, NULL, NULL);
	succeed_if (tmp == ELEKTRA_PLUGIN_STATUS_ERROR, "sendspec should return an error if spec and parentKey are NULL");

	keyDel (parentKey);
	ksDel (ks);
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

	test_sendspec ();

	print_result ("testmod_specload");

	return nbError;
}

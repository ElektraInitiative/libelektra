/**
 * @file
 *
 * @brief Tests for passwd plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

void test_read (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/passwd-read", ELEKTRA_KEY_VALUE, srcdir_file ("passwd/passwd_in"), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("system:/index", ELEKTRA_KEY_VALUE, "name", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("passwd");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet failed\n");
	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/passwd-read/root/uid", 0);
	succeed_if (key, "key root/uid not found\n");
	succeed_if (strcmp (elektraKeyString (key), "32") == 0, "root/uid doesn't match\n");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_write (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/passwd-write", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (30, elektraKeyNew ("system:/index", ELEKTRA_KEY_VALUE, "name", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("passwd");
	ElektraKeyset * ks = elektraKeysetNew (30, elektraKeyNew ("user:/tests/passwd-write/root", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/passwd-write/root/gecos", ELEKTRA_KEY_VALUE, "root", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/passwd-write/root/gid", ELEKTRA_KEY_VALUE, "1024", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/passwd-write/root/home", ELEKTRA_KEY_VALUE, "/root", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/passwd-write/root/passwd", ELEKTRA_KEY_VALUE, "x", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/passwd-write/root/shell", ELEKTRA_KEY_VALUE, "/bin/zsh", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/passwd-write/root/uid", ELEKTRA_KEY_VALUE, "1024", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "failed to write passwd file\n");
	succeed_if (compare_line_files (srcdir_file ("passwd/passwd_out"), elektraKeyString (parentKey)), "files do nat match as expected\n");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_read_write (void)
{
	ElektraKey * parentKeyRead = elektraKeyNew ("user:/tests/passwd-write", ELEKTRA_KEY_VALUE, srcdir_file ("passwd/passwd_in"), ELEKTRA_KEY_END);
	ElektraKey * parentKeyWrite = elektraKeyNew ("user:/tests/passwd-write", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("system:/index", ELEKTRA_KEY_VALUE, "name", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("passwd");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKeyRead) == 1, "kdbGet failed\n");
	elektraKeysetRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKeyWrite) == 1, "kdbSet failed\n");
	succeed_if (compare_line_files (srcdir_file ("passwd/passwd_in"), elektraKeyString (parentKeyWrite)), "files do not match as expected\n");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKeyRead);
	elektraKeyDel (parentKeyWrite);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("PASSWD     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_read ();
	test_write ();
	test_read_write ();

	print_result ("testmod_passwd");

	return nbError;
}

/**
 * @file
 *
 * @brief Tests for lock plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

#include <unistd.h>

#define TESTFILE "testfile"
#define TESTFILELOCK "testfile.lock"


static void test_filewrite ()
{
	printf ("test filewrite\n");

	Key * parentKey = keyNew ("user/tests/lock", KEY_VALUE, TESTFILE, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("lock");

	KeySet * ks = ksNew (0, KS_END);

	//test

	plugin->kdbGet (plugin, ks, parentKey);

	FILE * file = fopen (TESTFILELOCK, "r");
	succeed_if (file, "kdbGet dit not create a lock file");
	if (file)
	{
		fclose (file);
		remove (TESTFILELOCK);
	}

	//test

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_block ()
{
	printf ("test block\n");

	Key * parentKey = keyNew ("user/tests/lock", KEY_VALUE, TESTFILE, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("lock");

	KeySet * ks = ksNew (0, KS_END);

	//test

	plugin->kdbGet (plugin, ks, parentKey);

	pid_t pid = fork ();

	if (pid < 0)
	{
		fprintf (stderr, "ERROR: fork\n");
		remove (TESTFILELOCK);
		goto endblock;
	} else
	{
		if (pid)
		{
			//parent
			plugin->kdbGet (plugin, ks, parentKey);
		} else
		{
			//child
			remove (TESTFILELOCK);
			exit (EXIT_SUCCESS);
		}
	}
	remove (TESTFILELOCK);

	//test
endblock:
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_set ()
{
	printf ("test set\n");

	Key * parentKey = keyNew ("user/tests/lock", KEY_VALUE, TESTFILE, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("lock");

	KeySet * ks = ksNew (0, KS_END);

	//test

	FILE * file = fopen (TESTFILELOCK, "w");
	if (!file)
	{
		fprintf (stderr, "ERROR: can not wirte file %s\n",TESTFILELOCK);
		goto endset;
	}
	fclose (file);

	plugin->kdbSet (plugin, ks, parentKey);

	file = fopen (TESTFILELOCK, "r");
	succeed_if (!file, "kdbGet did not removed the lock file");
	if (file)
	{
		fclose (file);
		remove (TESTFILELOCK);
	}

	//test
endset:
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("LOCK     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_filewrite ();
	test_block ();
	test_set ();

	printf ("\ntestmod_lock RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

/**
 * @file
 *
 * @brief tests for jni module
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <testmod_jni.h>
#include <tests_plugin.h>

static void test_helloWorld (void)
{
	Key * parentKey = keyNew ("user:/tests/jni", KEY_VALUE, "", KEY_END);

	KeySet * conf;
	if (access (CLASSPATH, F_OK) == 0)
	{
		printf ("Using classpath '%s'.\n", CLASSPATH);
		conf = ksNew (20, keyNew ("system:/classpath", KEY_VALUE, CLASSPATH, KEY_END),
			      keyNew ("system:/print", KEY_VALUE, "ON", KEY_END),
			      keyNew ("system:/classname", KEY_VALUE, "org/libelektra/plugin/Return", KEY_END), KS_END);
	}
	else
	{
		fprintf (stderr, "Build JNA binding jar '%s' not found.\n", CLASSPATH);
		exit (-1);
	}

	PLUGIN_OPEN ("jni");

	KeySet * ks = ksNew (20, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 10, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 20, "call to kdbSet was not successful");
	succeed_if (plugin->kdbError (plugin, ks, parentKey) == 30, "call to kdbError was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet/kdbSet/kdbError");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet/kdbSet/kdbError");

	PLUGIN_CLOSE ();

	ksDel (ks);
	keyDel (parentKey);
}

int main (int argc, char ** argv)
{
	printf ("JNI          TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_helloWorld ();

	print_result ("test_jni");

	return nbError;
}

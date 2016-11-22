/**
 * @file
 *
 * @brief tests for jni module
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <testmod_jni.h>
#include <tests_plugin.h>

static void test_helloWorld ()
{
	Key * parentKey = keyNew ("user/tests/jni", KEY_VALUE, "", KEY_END);
	KeySet * conf =
		ksNew (20, keyNew ("system/classpath", KEY_VALUE, CLASSPATH, KEY_END), keyNew ("system/print", KEY_VALUE, "ON", KEY_END),
		       keyNew ("system/classname", KEY_VALUE, "elektra/plugin/Return", KEY_END), KS_END);
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

	printf ("\ntest_jni RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

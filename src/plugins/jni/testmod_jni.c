/**
 * @file
 *
 * @brief tests for jni module
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
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
	ElektraKey * parentKey = keyNew ("user:/tests/jni", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);

	ElektraKeyset * conf;
	if (access (CLASSPATH, F_OK) == 0)
	{
		printf ("Using classpath '%s'.\n", CLASSPATH);
		conf = ksNew (20, keyNew ("system:/classpath", ELEKTRA_KEY_VALUE, CLASSPATH, ELEKTRA_KEY_END),
			      keyNew ("system:/print", ELEKTRA_KEY_VALUE, "ON", ELEKTRA_KEY_END),
			      keyNew ("system:/classname", ELEKTRA_KEY_VALUE, "org/libelektra/plugin/Return", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	else
	{
		fprintf (stderr, "Build JNA binding jar '%s' not found.\n", CLASSPATH);
		exit (-1);
	}

	PLUGIN_OPEN ("jni");

	ElektraKeyset * ks = ksNew (20, ELEKTRA_KS_END);
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

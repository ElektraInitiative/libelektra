/**
 * @file
 *
 * @brief test suite for the fcrypt plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <kdb.h>
#include <kdbinternal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tests_internal.h>
#include <tests_plugin.h>

int main (int argc, char ** argv)
{
	printf ("FCRYPT       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	// TODO add unit tests here

	printf ("\n" ELEKTRA_PLUGIN_NAME " RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	return nbError;
}

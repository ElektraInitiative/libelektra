/**
 * @file
 *
 * @brief Tests for profile plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>


int main (int argc, char ** argv)
{
	printf ("PROFILE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	printf ("\ntestmod_profile RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

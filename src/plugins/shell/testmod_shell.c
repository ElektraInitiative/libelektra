/**
 * @file
 *
 * @brief Tests for shell plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>



int main (int argc, char ** argv)
{
	printf ("SHELL     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	printf ("\ntestmod_shell RESULTS: %d test(s) done. %d error(s).\n",
			nbTest, nbError);

	return nbError;
}


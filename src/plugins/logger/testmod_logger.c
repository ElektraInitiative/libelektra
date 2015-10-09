/**
* \file
*
* \brief Tests for logger plugin
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics()
{
}



int main(int argc, char** argv)
{
	printf ("LOGGER     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics();

	printf ("\ntestmod_logger RESULTS: %d test(s) done. %d error(s).\n",
			nbTest, nbError);

	return nbError;
}


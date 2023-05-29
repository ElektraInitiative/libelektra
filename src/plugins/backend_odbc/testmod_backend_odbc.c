/**
 * @file
 *
 * @brief Tests for the ODBC backend plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <tests_plugin.h>

// FIXME [new_backend]: add tests for ODBC backend


int main (int argc, char ** argv)
{
	printf ("ODBC-BACKEND     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	// FIXME [new_backend]: add tests
	/*
		test_simple ();
		test_default ();
		test_backref ();
	*/
	print_result ("testmod_backend_odbc");

	return nbError;
}

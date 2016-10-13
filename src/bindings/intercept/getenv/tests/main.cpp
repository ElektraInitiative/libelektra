/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <iostream>

namespace ckdb
{
extern "C" {
extern ckdb::KeySet * elektraDocu;
extern std::ostream * elektraLog;
}
void elektraSingleCleanup ();
}

int main (int argc, char ** argv)
{
	using namespace ckdb;
	::testing::InitGoogleTest (&argc, argv);
	int ret = RUN_ALL_TESTS ();
	elektraClose (); // valgrind does not detect cleanup outside main, so lets do it here
	elektraSingleCleanup ();
	return ret;
}

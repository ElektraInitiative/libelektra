/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <iostream>

namespace ckdb
{
extern "C" {
extern ckdb::KeySet * elektraDocu;
extern std::ostream * elektraLog;
}
void elektraSingleCleanup ();
} // namespace ckdb

int main (int argc, char ** argv)
{
	using namespace ckdb;
	::testing::InitGoogleTest (&argc, argv);
	int ret = RUN_ALL_TESTS ();
	elektraClose (); // valgrind does not detect cleanup outside main, so lets do it here
	elektraSingleCleanup ();
	return ret;
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <iostream>
#include <kdb.hpp>
#include <keysetio.hpp>

int main ()
{
	using namespace kdb;
	KDB kdb;
	KeySet conf;
	kdb.get (conf, "/");
	std::cout << conf;
	kdb.set (conf, "/");
}

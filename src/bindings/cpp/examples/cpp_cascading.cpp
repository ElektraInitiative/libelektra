/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

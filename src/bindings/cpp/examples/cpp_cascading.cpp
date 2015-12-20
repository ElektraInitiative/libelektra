/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdb.hpp>
#include <keysetio.hpp>
#include <iostream>

int main()
{
	using namespace kdb;
	KDB kdb;
	KeySet conf;
	kdb.get(conf, "/");
	std::cout << conf;
	kdb.set(conf, "/");
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <kdb.hpp>

#include <keyio.hpp>

using namespace kdb;

int main ()
{
	KeySet config;
	KDB kdb;
	kdb.get (config, "/sw/MyApp");

	Key k = config.lookup ("/sw/MyApp/mykey");
	if (k)
	{
		std::cout << k << " is " << k.get<int> () << std::endl;
	}
	else
	{
		std::cerr << "No key found" << std::endl;
		return 1;
	}
}

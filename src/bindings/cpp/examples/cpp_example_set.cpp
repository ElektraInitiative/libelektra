/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.hpp>

#include <iostream>

using namespace kdb;

int main ()
{
	KeySet config;
	KDB kdb;
	kdb.get (config, "/sw/MyApp");

	Key k = config.lookup ("/sw/MyApp/mykey");
	if (k)
	{
		k.set<int> (k.get<int> () + 1);
	}
	else
	{
		Key n;
		n.setName ("user:/sw/MyApp/mykey");
		n.set<int> (0);
		config.append (n);
	}

	kdb.set (config, "/sw/MyApp");
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <backend.hpp>
#include <backends.hpp>

#include <iostream>

int main()
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b;
	b.setMountpoint(Key("/", KEY_CASCADING_NAME, KEY_END), KeySet(0, KS_END));
	b.addPlugin("resolver");
	b.addPlugin("dump");
	b.useConfigFile("file.ecf");
	b.validated();

	KeySet mountConfig;
	b.serialize(mountConfig);

	mountConfig.rewind();
	while (mountConfig.next())
	{
		std::cout << mountConfig.current().getName()
			  << " = " << mountConfig.current().getString()
			  << std::endl;
	}
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <backendbuilder.hpp>
#include <backends.hpp>

#include <iostream>

int main ()
{
	using namespace kdb;
	using namespace kdb::tools;
	MountBackendBuilder b;
	b.setMountpoint (Key ("/", KEY_END), KeySet (0, KS_END));
	b.addPlugin (PluginSpec ("resolver"));
	b.addPlugin (PluginSpec ("dump"));
	b.useConfigFile ("file.ecf");
	b.validated ();

	KeySet mountConfig;
	b.serialize (mountConfig);

	for (ssize_t it = 0; it < mountConfig.size(); ++it)
	{
		std::cout << mountConfig.at (it).getName () << " = " << mountConfig.at (it).getString () << std::endl;
	}
}

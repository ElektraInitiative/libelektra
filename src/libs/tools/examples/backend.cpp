/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <backendbuilder.hpp>
#include <backends.hpp>

#include <iostream>

int main ()
{
	using namespace kdb;
	using namespace kdb::tools;
	MountBackendBuilder b;
	b.setMountpoint (Key ("/", KEY_CASCADING_NAME, KEY_END), KeySet (0, KS_END));
	b.addPlugin (PluginSpec ("resolver"));
	b.addPlugin (PluginSpec ("dump"));
	b.useConfigFile ("file.ecf");
	b.validated ();

	KeySet mountConfig;
	b.serialize (mountConfig);

	mountConfig.rewind ();
	while (mountConfig.next ())
	{
		std::cout << mountConfig.current ().getName () << " = " << mountConfig.current ().getString () << std::endl;
	}
}

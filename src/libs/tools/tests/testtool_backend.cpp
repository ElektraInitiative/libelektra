/**
 * @file
 *
 * @brief Tests for the Backend class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <backend.hpp>
#include <backends.hpp>

#include <gtest/gtest.h>
#include <iostream>
#include <kdb.hpp>
#include <internal/kdb/config.h>
#include <string>

// FIXME [new_backend]: tests disabled, mount tooling will be rewritten in C

std::string makeLiteralString (std::string str)
{
	std::string ret;
	for (auto & elem : str)
	{
		if (elem == '\\')
		{
			ret += "\\\\";
		}
		else
		{
			ret += elem;
		}
	}
	return ret;
}

/**
 * @brief Easily allows one to generate regression tests for keysets.
 *
 * @param tocheck the keyset to check (name + string)
 * @param name the name of the keyset
 */
void outputGTest (kdb::KeySet tocheck, std::string name)
{
	for (kdb::Key k : tocheck)
	{
		std::cout << name << "foreach key in keyset;" << std::endl;
		std::cout << "EXPECT_EQ (" << name << "k.getName (), \"" << makeLiteralString (k.getName ())
			  << "\") << \"name of element in keyset wrong\";" << std::endl;
		std::cout << "EXPECT_EQ (" << name << "k.getString (), \"" << makeLiteralString (k.getString ())
			  << "\") << \"string of element in keyset wrong\";" << std::endl;
	}
}

TEST (Backend, DISABLED_backendName)
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b;
	b.setMountpoint (Key ("user:/a", KEY_END), KeySet (0, KS_END));
	EXPECT_EQ (b.getMountpoint (), "user:/a");
}

TEST (Backend, DISABLED_SimpleBackend)
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b;
	b.setMountpoint (Key ("/", KEY_END), KeySet (0, KS_END));
	EXPECT_EQ (b.getMountpoint (), "/");
	b.addPlugin (PluginSpec ("resolver"));
	return;
	b.addPlugin (PluginSpec ("dump"));
	b.useConfigFile ("abc");
	EXPECT_TRUE (b.validated ());

	KeySet mountConfig;
	b.serialize (mountConfig);

	// outputGTest (mountConfig, "mountConfig");
	ssize_t it = 0;
	EXPECT_EQ (mountConfig.at (it).getName (), "system:/elektra/mountpoints/\\/") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (),
		   "This is a configuration for a backend, see subkeys for more information") // fails (empty string returned)
		<< "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//config") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//config/fcrypt/textmode")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "0") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//config/mountpoint") // fails ('/definition/path' instead of '/config/mountpoint')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "/") << "string of element in keyset wrong"; // fails ('abc' instead of '/')

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//config/path")
		<< "name of element in keyset wrong"; // fails ('/definition/positions/get/resolver' instead of '/config/path')
	EXPECT_EQ (mountConfig.at (it).getString (), "abc") << "string of element in keyset wrong"; // fails ('resolver' instead of
												    // 'abc')

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//error")
		<< "name of element in keyset wrong"; // fails ('/defintion/positions/get/storage' instead of '/error')
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong"; // fails ('dump' instead of '')
	EXPECT_EQ (
		mountConfig.at (++it).getName (),
		"system:/elektra/mountpoints/\\//error/rollback") // fails ('/definition/positions/set/commit' instead of '/error/rollback')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong"; // fails ('resolver' instead of '')
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//error/rollback/#0") // fails ('/definition/positions/set/resolver' instead of
									// '/error/rollback/#0')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong"; // fails ('resolver' instead of '')
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//error/rollback/#0/label") // fails ('/definition/positions/set/rollback' instead of
									      // '/error/rollback/#0/label')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "resolver") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//error/rollback/#0/name") // fails ('/definition/positions/set/storage' instead of
									     // '/error/rollback/#0/name')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), KDB_DEFAULT_RESOLVER)
		<< "string of element in keyset wrong"; // fails ('dump' instead of 'resolver_fm_hpu_b')
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//get")
		<< "name of element in keyset wrong"; // fails ('/plugins/backend' instead of '/get')
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//get/getresolver") // fails ('/plugins/backend/name' instead of '/get/resolver')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong"; // fails ('backend' instead of '')
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//get/getresolver/#0") // fails ('/plugins/dump' instead of '/get/resolver/#0')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//get/getresolver/#0/reference") // fails ('/plugins/dump/name' instead of
										   // '/get/getresolver/#0/reference')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "resolver")
		<< "string of element in keyset wrong"; // fails ('dump' instead of 'resolver')
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//get/getstorage") // fails ('/plugins/resolver' instead of '/get/getstorage')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//get/getstorage/#0") // fails ('/plugins/resolver/name' instead of '/get/getstorage/#0')
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "")
		<< "string of element in keyset wrong"; // fails ('resolver_fm_hpu_b' instead of '')
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\//get/getstorage/#0/label") // Exception (method call on a null-key!)S
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "dump") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//get/getstorage/#0/name")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "dump") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/commit") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/commit/#0") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/commit/#0/reference")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "resolver") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/setresolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/setresolver/#0")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/setresolver/#0/reference")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "resolver") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/setstorage") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/setstorage/#0")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\//set/setstorage/#0/reference")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "dump") << "string of element in keyset wrong";
}

TEST (Backend, DISABLED_CrazyName)
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b;
	b.setMountpoint (Key ("/crazy///.//name/../a..__.b/._.///._c__d", KEY_END), KeySet (0, KS_END));
	b.addPlugin (PluginSpec ("resolver"));
	b.addPlugin (PluginSpec ("dump"));

	EXPECT_TRUE (b.validated ());

	KeySet mountConfig;
	b.serialize (mountConfig);

	// outputGTest (mountConfig, "mountConfig");

	ssize_t it = 0;
	EXPECT_EQ (mountConfig.at (it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "This is a configuration for a backend, see subkeys for more information")
		<< "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/config/fcrypt/textmode")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "0") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/config/path")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/errorplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/getplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/getplugins/#0#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/getplugins/#5#dump#dump#")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/mountpoint")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "/crazy/a..__.b/._./._c__d") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/setplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/setplugins/#0#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/setplugins/#5#dump")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/\\/crazy\\/a..__.b\\/._.\\/._c__d/setplugins/#7#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
}

TEST (Backend, DISABLED_SimpleBackendWithConf)
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b;
	b.setMountpoint (Key ("user:/somewhere", KEY_END), KeySet (0, KS_END));
	EXPECT_EQ (b.getMountpoint (), "user:/somewhere");
	KeySet backendConf (5, *Key ("system:/globalConf", KEY_VALUE, "for everywhere", KEY_END),
			    *Key ("system:/other_global_conf", KEY_VALUE, "more", KEY_END), KS_END);
	b.setBackendConfig (backendConf);
	KeySet resConf (5, *Key ("user:/res_conf", KEY_VALUE, "do resolving", KEY_END),
			*Key ("user:/other_res_conf", KEY_VALUE, "do resolving too", KEY_END), KS_END);
	b.addPlugin (PluginSpec ("resolver", resConf));
	KeySet dumpConf (5, *Key ("user:/file_format", KEY_VALUE, "1", KEY_END),
			 *Key ("user:/other_dump_conf", KEY_VALUE, "some dump config", KEY_END), KS_END);
	b.addPlugin (PluginSpec ("dump", dumpConf));
	b.useConfigFile ("abc");
	EXPECT_TRUE (b.validated ());

	KeySet mountConfig;
	b.serialize (mountConfig);

	// outputGTest (mountConfig, "mountConfig");

	ssize_t it = 0;
	EXPECT_EQ (mountConfig.at (it).getName (), "system:/elektra/mountpoints/user:\\/somewhere") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "This is a configuration for a backend, see subkeys for more information")
		<< "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/fcrypt/textmode")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "0") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/globalConf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "for everywhere") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/other_global_conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "more") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/path")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "abc") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/errorplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#/config/other_res_conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "do resolving too") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#/config/res_conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "do resolving") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#0#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#dump#dump#")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#dump#dump#/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#dump#dump#/config/file_format")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "1") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#dump#dump#/config/other_dump_conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "some dump config") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/mountpoint")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "user:/somewhere") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#0#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#5#dump")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#7#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
}


TEST (Backend, DISABLED_SimpleBackendWithNeededConf)
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b;
	b.setMountpoint (Key ("user:/somewhere", KEY_END), KeySet (0, KS_END));
	EXPECT_EQ (b.getMountpoint (), "user:/somewhere");
	KeySet backendConf (5, *Key ("system:/globalConf", KEY_VALUE, "for everywhere", KEY_END),
			    *Key ("system:/struct/FStab/device", KEY_VALUE, "loses, not in key", KEY_END), KS_END);
	b.setBackendConfig (backendConf);
	KeySet resConf (5, *Key ("user:/res_conf", KEY_VALUE, "do resolving", KEY_END),
			*Key ("user:/other_res_conf", KEY_VALUE, "do resolving too", KEY_END), KS_END);
	b.addPlugin (PluginSpec ("resolver", resConf));
	KeySet dumpConf (5, *Key ("user:/file_format", KEY_VALUE, "1", KEY_END),
			 *Key ("user:/other_dump_conf", KEY_VALUE, "some dump config", KEY_END), KS_END);
	try
	{
		b.addPlugin (PluginSpec ("fstab", dumpConf));
	}
	catch (...)
	{
		std::cout << "No fstab plugin, abort test case" << std::endl;
		return;
	}
	b.useConfigFile ("abc");
	EXPECT_TRUE (b.validated ());

	KeySet mountConfig;
	b.serialize (mountConfig);

	// outputGTest (mountConfig, "mountConfig");

	ssize_t it = 0;
	EXPECT_EQ (mountConfig.at (it).getName (), "system:/elektra/mountpoints/user:\\/somewhere") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "This is a configuration for a backend, see subkeys for more information")
		<< "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "The configuration which is needed") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/globalConf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "for everywhere") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/path")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "abc") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/struct")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "list FStab") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/struct/FStab")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/struct/FStab/device")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/struct/FStab/dumpfreq")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/struct/FStab/mpoint")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/struct/FStab/options")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/struct/FStab/passno")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/struct/FStab/type")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/errorplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#/config/other_res_conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "do resolving too") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#" KDB_DEFAULT_RESOLVER "#resolver#/config/res_conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "do resolving") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#0#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#fstab#fstab#")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#fstab#fstab#/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#fstab#fstab#/config/file_format")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "1") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#fstab#fstab#/config/other_dump_conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "some dump config") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/mountpoint")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "user:/somewhere") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#0#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#5#fstab")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#7#resolver")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
}


TEST (Backend, DISABLED_SimpleBackendWithUnderscore)
{
	using namespace kdb;
	using namespace kdb::tools;
	Backend b;
	b.setMountpoint (Key ("user:/somewhere", KEY_END), KeySet (0, KS_END));
	EXPECT_EQ (b.getMountpoint (), "user:/somewhere");
	KeySet backendConf (5, *Key ("system:/global/conf", KEY_VALUE, "for backend", KEY_END), KS_END);
	b.setBackendConfig (backendConf);
	KeySet resConf (5, *Key ("user:/res/conf", KEY_VALUE, "do it", KEY_END), KS_END);
	KeySet dumpConf (5, *Key ("user:/something", KEY_VALUE, "a val", KEY_END), KS_END);
	try
	{
		b.addPlugin (PluginSpec ("dump", resConf));
		b.addPlugin (PluginSpec ("resolver_fm_b_b", dumpConf));
	}
	catch (...)
	{
		std::cout << "No resolver_fm_b_b plugin (or dump), abort test case" << std::endl;
		return;
	}
	b.useConfigFile ("abc");
	EXPECT_TRUE (b.validated ());

	KeySet mountConfig;
	b.serialize (mountConfig);

	// outputGTest (mountConfig, "mountConfig");

	ssize_t it = 0;
	EXPECT_EQ (mountConfig.at (it).getName (), "system:/elektra/mountpoints/user:\\/somewhere") << "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "This is a configuration for a backend, see subkeys for more information")
		<< "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/fcrypt/textmode")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "0") << "string of element in keyset wrong";

	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/global/conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "for backend") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/config/path")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "abc") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/errorplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#resolver_fm_b_b#resolver_fm_b_b#")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#resolver_fm_b_b#resolver_fm_b_b#/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/errorplugins/#5#resolver_fm_b_b#resolver_fm_b_b#/config/something")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "a val") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#0#resolver_fm_b_b")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#dump#dump#")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#dump#dump#/config")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (),
		   "system:/elektra/mountpoints/user:\\/somewhere/getplugins/#5#dump#dump#/config/res/conf")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "do it") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/mountpoint")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "user:/somewhere") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#0#resolver_fm_b_b")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#5#dump")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (++it).getName (), "system:/elektra/mountpoints/user:\\/somewhere/setplugins/#7#resolver_fm_b_b")
		<< "name of element in keyset wrong";
	EXPECT_EQ (mountConfig.at (it).getString (), "") << "string of element in keyset wrong";
}

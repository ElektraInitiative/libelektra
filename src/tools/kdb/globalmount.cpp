/**
 * @file
 *
 * @brief source file of spec mount command
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */


#include <cmdline.hpp>
#include <globalmount.hpp>
#include <specreader.hpp>

#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

GlobalMountCommand::GlobalMountCommand ()
{
}

void GlobalMountCommand::buildBackend (Cmdline const & cl)
{
	GlobalPluginsBuilder backend;

	// TODO: not yet implemented:
	// backend.setBackendConfig(cl.getPluginsConfig("system/"));
	backend.addPlugins (parseArguments (cl.globalPlugins));
	backend.addPlugins (parseArguments (cl.arguments.begin (), cl.arguments.end ()));

	// Call it a day
	outputMissingRecommends (backend.resolveNeeds (cl.withRecommends));
	mountConf.cut (Key (GlobalPluginsBuilder::globalPluginsPath, KEY_END));
	backend.serialize (mountConf);
}

/**
 * @brief Its quite linear whats going on, but there are many steps involved
 *
 * @param cl the commandline
 *
 * @retval 0 on success (otherwise exception)
 */
int GlobalMountCommand::execute (Cmdline const & cl)
{
	mountpointsPath = GlobalPluginsBuilder::globalPluginsPath;
	readMountConf (cl);

	buildBackend (cl);
	doIt ();

	return 0;
}

GlobalMountCommand::~GlobalMountCommand ()
{
}

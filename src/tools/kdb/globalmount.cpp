/**
 * @file
 *
 * @brief source file of spec mount command
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <globalmount.hpp>
#include <cmdline.hpp>
#include <specreader.hpp>

#include <iostream>
#include <iterator>
#include <fstream>
#include <vector>
#include <string>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

GlobalMountCommand::GlobalMountCommand()
{}

void GlobalMountCommand::buildBackend (Cmdline const& cl)
{
	GlobalPluginsBuilder backend;

	// TODO: impl globalPlugins conf param
	// backend.addPlugins (parseArguments (cl.globalPlugins));
	backend.addPlugins (parseArguments (cl.arguments.begin(), cl.arguments.end()));

	// Call it a day
	outputMissingRecommends(backend.resolveNeeds(cl.withRecommends));
	mountConf.cut (Key("system/elektra/globalplugins", KEY_END));
	backend.serialize (mountConf);
}

/**
 * @brief Its quite linear whats going on, but there are many steps involved
 *
 * @param cl the commandline
 *
 * @retval 0 on success (otherwise exception)
 */
int GlobalMountCommand::execute (Cmdline const& cl)
{
	mountpointsPath = GlobalPluginsBuilder::globalPluginsPath;
	readMountConf(cl);

	buildBackend(cl);
	doIt();

	return 0;
}

GlobalMountCommand::~GlobalMountCommand()
{}

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

std::vector<std::string> GlobalMountCommand::getMtab ()
{
	std::vector<std::string> ret;
	Key globalPluginsKey ("system/elektra/globalplugins/postcommit/user/plugins", KEY_END);

	mountConf.rewind ();
	Key currentPluginConfig ("proc/notbelow", KEY_END);
	for (auto const & key : mountConf)
	{
		if (key.isDirectBelow (globalPluginsKey))
		{
			ret.push_back (key.getString ());
			currentPluginConfig = key.dup ();
			currentPluginConfig.addBaseName ("config");
		}
		else if (key.isBelow (currentPluginConfig))
		{
			ret.push_back (key.getName ().substr (currentPluginConfig.getName ().size () + 1) + "=" + key.getString ());
		}
	}
	return ret;
}


void GlobalMountCommand::outputMtab (Cmdline const &)
{
	bool first = true;
	auto const & mtab = getMtab ();
	for (auto const & mtabEntry : mtab)
	{
		if (mtabEntry.find ('=') != std::string::npos)
		{
			std::cout << " ";
		}
		else if (!first)
		{
			std::cout << "\n";
		}
		std::cout << mtabEntry;
		first = false;
	}
	if (!first) std::cout << endl;
}

namespace
{
void removePlugins (std::vector<std::string> & plugins, std::string globalPlugins)
{
	std::istringstream iss (globalPlugins);
	std::string plugin;
	while (iss >> plugin)
	{
		auto const & it = std::find (plugins.begin (), plugins.end (), plugin);
		if (it != plugins.end ())
		{
			plugins.erase (it); // remove plugin
			// and remove its config:
			while (it != plugins.end () && it->find ("=") != std::string::npos)
			{
				std::cout << "configs " << *it << std::endl;
				plugins.erase (it);
			}
		}
	}
}
}

void GlobalMountCommand::buildBackend (Cmdline const & cl)
{
	GlobalPluginsBuilder backend;

	// TODO: not yet implemented:
	// backend.setBackendConfig(cl.getPluginsConfig("system/"));
	backend.addPlugins (parseArguments (cl.globalPlugins));
	backend.addPlugins (parseArguments (cl.arguments.begin (), cl.arguments.end ()));

	auto mtab = getMtab ();
	removePlugins (mtab, cl.globalPlugins); // do not readd again
	backend.addPlugins (parseArguments (mtab.begin (), mtab.end ()));

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

	if (!cl.interactive && cl.arguments.empty ())
	{
		// no interactive mode, so lets output the mtab
		outputMtab (cl);
		return 0;
	}


	buildBackend (cl);
	doIt ();

	return 0;
}

GlobalMountCommand::~GlobalMountCommand ()
{
}

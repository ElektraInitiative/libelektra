/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <list.hpp>

#include <iostream>

#include <cmdline.hpp>
#include <plugindatabase.hpp>

using namespace kdb;
using namespace std;

ListCommand::ListCommand()
{}

int ListCommand::execute(Cmdline const& cl)
{
	using namespace kdb::tools;
	ModulesPluginDatabase db;

	std::vector<std::string> plugins = db.listAllPlugins();

	std::multimap<int, std::string> sortedPlugins;
	for (const auto & plugin : plugins)
	{
		int s = db.calculateStatus(db.lookupInfo (PluginSpec(plugin, KeySet(5, *Key("system/module",
			KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END)), "status"));
		sortedPlugins.insert (std::make_pair (s, plugin));
	}

	if (cl.verbose) cout << "number of all plugins: " << plugins.size() << endl;

	for (auto & plugin : sortedPlugins)
	{
		std::cout << plugin.second;
		if (cl.verbose)
		{
			std::cout << " " << plugin.first;
		}

		if (cl.null)
		{
			cout << '\0';
		}
		else
		{
			cout << endl;
		}
	}

	return 0;
}

ListCommand::~ListCommand()
{}

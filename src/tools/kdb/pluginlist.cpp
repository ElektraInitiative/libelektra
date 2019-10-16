/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <pluginlist.hpp>

#include <algorithm>
#include <iostream>

#include <cmdline.hpp>
#include <plugindatabase.hpp>

using namespace kdb;
using namespace std;

PluginListCommand::PluginListCommand ()
{
}

int PluginListCommand::execute (Cmdline const & cl)
{
	using namespace kdb::tools;

	if (cl.arguments.size () > 1)
	{
		throw invalid_argument ("too many arguments, use 1 argument (provider) or no arguments (all plugins)");
		;
	}

	ModulesPluginDatabase db;
	std::vector<std::string> plugins;
	if (cl.arguments.size () == 1)
	{
		std::vector<PluginSpec> pluginspecs = db.lookupAllProvides (cl.arguments[0]);
		plugins.resize (pluginspecs.size ());
		std::transform (pluginspecs.begin (), pluginspecs.end (), plugins.begin (),
				[](PluginSpec const & ps) { return ps.getName (); });
	}
	else
	{
		plugins = db.listAllPlugins ();
	}

	std::multimap<int, std::string> statusPlugins;
	for (const auto & plugin : plugins)
	{
		try
		{
			int s = db.calculateStatus (db.lookupInfo (
				PluginSpec (plugin,
					    KeySet (5,
						    *Key ("system/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END),
						    KS_END)),
				"status"));
			statusPlugins.insert (std::make_pair (s, plugin));
		}
		catch (std::exception const & e)
		{
			statusPlugins.insert (std::make_pair (-1000000, plugin));
			if (cl.verbose)
			{
				std::cerr << "No status found for " << plugin << std::endl;
			}
		}
	}

	if (cl.verbose) cout << "number of all plugins: " << plugins.size () << endl;

	std::vector<std::string> sortedPlugins;

	for (auto & plugin : statusPlugins)
	{
		std::string elem = plugin.second;
		if (cl.verbose)
		{
			elem += " " + std::to_string (plugin.first);
		}
		sortedPlugins.push_back (elem);
	}

	std::sort (sortedPlugins.begin (), sortedPlugins.end ());

	for (auto & elem : sortedPlugins)
	{
		std::cout << elem;
		if (cl.null)
		{
			std::cout << '\0';
		}
		else
		{
			std::cout << endl;
		}
	}

	return 0;
}

PluginListCommand::~PluginListCommand ()
{
}

/**
 * @file
 *
 * @brief Implementation of PluginDatabase(s)
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <plugindatabase.hpp>

#include <modules.hpp>

#include <algorithm>
#include <kdbconfig.h>

#ifdef HAVE_GLOB
#include <glob.h>
#endif

namespace kdb
{

namespace tools
{

class ModulesPluginDatabase::Impl
{
public:
	Impl () {}
	~Impl () {}
	Modules modules;
};

ModulesPluginDatabase::ModulesPluginDatabase () :
	impl(new ModulesPluginDatabase::Impl())
{}

ModulesPluginDatabase::~ModulesPluginDatabase ()
{}

std::vector<std::string> ModulesPluginDatabase::listAllPlugins() const
{
	std::vector<std::string> ret;
#ifdef ELEKTRA_SHARED
#ifdef HAVE_GLOB
	glob_t pglob;
	if (glob(BUILTIN_PLUGIN_FOLDER "/libelektra-*", GLOB_NOSORT, NULL, &pglob) == 0)
	{
		for (size_t i=0; i<pglob.gl_pathc; ++i)
		{
			std::string fn (pglob.gl_pathv[i]);
			size_t start = fn.find_last_of('-');
			if (start == std::string::npos) continue; // ignore wrong file
			size_t end = fn.find_last_of('.');
			if (end == std::string::npos) continue; // ignore wrong file
			ret.push_back(fn.substr(start+1, end-start-1));
		}
	}
#endif
	if (!ret.empty())
	{
		std::sort (ret.begin(), ret.end());
		return ret;
	}
	// if we did not find plugins, return buildinPlugins
	// (even if they might be wrong for ELEKTRA_SHARED)
#endif
	std::string buildinPlugins = ELEKTRA_PLUGINS;
	std::istringstream ss(buildinPlugins);
	std::string plugin;
	while (getline(ss, plugin, ';'))
	{
		ret.push_back(plugin);
	}
	// remove duplicates:
	std::sort (ret.begin(), ret.end());
	ret.erase (std::unique (ret.begin(), ret.end()), ret.end());
	return ret;
}


std::string ModulesPluginDatabase::lookupInfo (PluginSpec const & spec, std::string const & which) const
{
	PluginPtr plugin = impl->modules.load (spec.getName(), spec.getConfig());
	return plugin->lookupInfo (which);
}

namespace
{

// TODO: directly use data from CONTRACT.ini
const std::map <std::string, int> statusMap=
{
	{"popular", 4000},
	{"productive", 2000},
	{"specific", 1000},
	{"unittest", 500},
	{"tested", 250},
	{"preview", -50},
	{"memleak", -250},
	{"experimental", -500},
	{"unfinished", -1000},
	{"concept", -2000},
	{"discouraged", -4000}

};

int calculateStatus (std::string statusString)
{
	int ret = 0;
	std::istringstream ss (statusString);
	std::string status;
	while (ss >> status)
	{
		auto it = statusMap.find(status);
		if (it != statusMap.end())
		{
			ret += it->second;
		}
		else
		{
			ret += stoi(status);
		}
	}
	return ret;
}

}

PluginSpec ModulesPluginDatabase::lookupMetadata (std::string const & which) const
{
	std::vector<std::string> allPlugins = listAllPlugins();
	std::map<int, PluginSpec> foundPlugins;

	// collect possible plugins
	for (auto const & plugin : allPlugins)
	{
		try {
			// TODO remove /module hack
			std::istringstream ss (lookupInfo (PluginSpec(plugin, KeySet(5, *Key("system/module",
				KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END)),
				"metadata"));
			std::string metadata;
			while (ss >> metadata)
			{
				if (metadata == which)
				{
					int status = calculateStatus(lookupInfo (PluginSpec(plugin, KeySet(5, *Key("system/module",
						KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END)),
						"status"));
					foundPlugins.insert(std::make_pair(status, PluginSpec(plugin)));
					break;
				}
			}
		} catch (...) { } // assume not loaded
	}

	if (foundPlugins.empty())
	{
		throw NoPlugin ("Could not find plugin with metadata " + which);
	}

	// the largest element of the map contains the best-suited plugin:
	return foundPlugins.rbegin()->second;
}

PluginSpec ModulesPluginDatabase::lookupProvides (std::string const & which) const
{
	std::vector<std::string> allPlugins = listAllPlugins();
	std::map<int, PluginSpec> foundPlugins;

	// check if plugin with this name exists
	// TODO: needed even if virtual are handled separately?
	auto it = std::find(allPlugins.begin(), allPlugins.end(), which);
	if (it != allPlugins.end())
	{
		return PluginSpec(which);
	}

	for (auto const & plugin : allPlugins)
	{
		if (plugin == which)
		{
			return PluginSpec(plugin);
		}

		// TODO: make sure (non)-equal plugins (i.e. with same/different contract) are handled correctly
		try {
			if (lookupInfo (PluginSpec(plugin, KeySet(5, *Key("system/module",
				KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END)),
				"provides") == which)
			{
				int status = calculateStatus(lookupInfo (PluginSpec(plugin, KeySet(5, *Key("system/module",
					KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END)),
					"status"));
				foundPlugins.insert(std::make_pair(status, PluginSpec(plugin)));
			}
		} catch (...) { } // assume not loaded
	}

	if (foundPlugins.empty())
	{
		throw NoPlugin("No plugin that provides " + which + " could be found");
	}

	// the largest element of the map contains the best-suited plugin:
	return foundPlugins.rbegin()->second;
}



std::vector<std::string> MockPluginDatabase::listAllPlugins() const
{
	std::vector<std::string> plugins;
	for (auto const & plugin : data)
	{
		plugins.push_back(plugin.first.getName());
	}
	return plugins;
}

std::string MockPluginDatabase::lookupInfo(PluginSpec const & spec, std::string const & which) const
{
	auto it = data.find(spec);
	if (it != data.end())
	{
		return it->second[which];
	}

	return "";
}

}

}

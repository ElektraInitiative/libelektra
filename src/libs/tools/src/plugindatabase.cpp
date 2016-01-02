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

/**
 * In the shared case, it will search for shared libraries, and if not found
 * any, it will fallback to internal list (plugins that were compiled).
 *
 * @return a list of all available plugins
 */
std::vector<std::string> listAllAvailablePlugins()
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

std::string ModulesPluginDatabase::lookupInfo (PluginSpec const & spec, std::string const & which) const
{
	PluginPtr plugin = impl->modules.load (spec.name, spec.config);
	return plugin->lookupInfo (which);
}

PluginSpec ModulesPluginDatabase::lookupMetadata (std::string const & which) const
{
	// TODO: implement
	return PluginSpec(which);
}

PluginSpec ModulesPluginDatabase::lookupProvides (std::string const & which) const
{
	std::vector<std::string> allPlugins = listAllAvailablePlugins();

	// check if plugin with this name exists
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

		// TODO: improve search strategy
		// TODO: make sure (non)-equal plugins (i.e. with same/different contract) are handled correctly
		try {
			if (lookupInfo (PluginSpec(plugin, KeySet(5, *Key("system/module",
				KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END)),
				"provides") == which)
			{
				return PluginSpec(plugin);
			}
		} catch (...) { } // assume not loaded
	}

	throw NoPlugin("No plugin " + which + " could be found");
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

PluginSpec MockPluginDatabase::lookupMetadata (std::string const & which) const
{
	for (auto const & plugin : data)
	{
		if (lookupInfo (plugin.first, "metadata") == which)
		{
			return plugin.first;
		}
	}

	throw NoPlugin("No plugin that implements metadata " + which + " could be found");
}

PluginSpec MockPluginDatabase::lookupProvides (std::string const & which) const
{
	for (auto const & plugin : data)
	{
		if (plugin.first.name == which)
		{
			return plugin.first;
		}

		if (lookupInfo (plugin.first, "provides") == which)
		{
			return plugin.first;
		}
	}

	throw NoPlugin("No plugin that implements provider " + which + " could be found");
}

}

}


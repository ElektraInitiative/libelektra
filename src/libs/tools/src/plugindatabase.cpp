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
	return plugin->lookupInfo(which);
}

PluginSpec ModulesPluginDatabase::lookupProvides (std::string const & which) const
{
	std::vector<std::string> allPlugins = listAllAvailablePlugins();

	for (auto const & plugin : allPlugins)
	{
		if (plugin == which)
		{
			return plugin;
		}

		// TODO: improve search strategy
		if (lookupInfo (PluginSpec(plugin), "provides") == which)
		{
			return plugin;
		}
	}

	throw NoPlugin("No plugin " + which + " could be found");
}

}

}


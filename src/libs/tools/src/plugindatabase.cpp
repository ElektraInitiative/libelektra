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

#include <set>

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
	Impl ()
	{
	}
	~Impl ()
	{
	}
	Modules modules;
};

ModulesPluginDatabase::ModulesPluginDatabase () : impl (new ModulesPluginDatabase::Impl ())
{
}

ModulesPluginDatabase::~ModulesPluginDatabase ()
{
}

std::vector<std::string> ModulesPluginDatabase::listAllPlugins () const
{
	std::vector<std::string> ret;
#ifdef ELEKTRA_SHARED
#ifdef HAVE_GLOB
	std::set<std::string> toIgnore = {
		"proposal", "core", "ease", "meta", "plugin", "full", "kdb", "static",
	};
	glob_t pglob;
	if (glob (BUILTIN_PLUGIN_FOLDER "/libelektra-*", GLOB_NOSORT, NULL, &pglob) == 0)
	{
		for (size_t i = 0; i < pglob.gl_pathc; ++i)
		{
			std::string fn (pglob.gl_pathv[i]);
			size_t start = fn.find_last_of ('-');
			if (start == std::string::npos) continue; // ignore wrong file
			std::string name = fn.substr (start + 1);
			size_t end = fn.find_first_of ('.');
			name = name.substr (0, end - start - 1);
			if (end == std::string::npos) continue;		       // ignore wrong file
			if (toIgnore.find (name) != toIgnore.end ()) continue; // ignore
			ret.push_back (name);
		}
	}
#endif
	if (!ret.empty ())
	{
		std::sort (ret.begin (), ret.end ());
		return ret;
	}
// if we did not find plugins, return buildinPlugins
// (even if they might be wrong for ELEKTRA_SHARED)
#endif
	std::string buildinPlugins = ELEKTRA_PLUGINS;
	std::istringstream ss (buildinPlugins);
	std::string plugin;
	while (getline (ss, plugin, ';'))
	{
		ret.push_back (plugin);
	}
	// remove duplicates:
	std::sort (ret.begin (), ret.end ());
	ret.erase (std::unique (ret.begin (), ret.end ()), ret.end ());
	return ret;
}


namespace
{

bool hasProvides (PluginDatabase const & pd, std::string which)
{
	std::vector<std::string> allPlugins = pd.listAllPlugins ();
	std::map<int, PluginSpec> foundPlugins;

	for (auto const & plugin : allPlugins)
	{
		std::istringstream ss (pd.lookupInfo (
			PluginSpec (
				plugin,
				KeySet (5, *Key ("system/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END)),
			"provides"));
		std::string provide;
		while (ss >> provide)
		{
			if (provide == which)
			{
				return true;
			}
		}
	}
	return false;
}
}


// TODO: directly use data from CONTRACT.ini
const std::map<std::string, int> PluginDatabase::statusMap = {
	// clang-format off
   {"default",      64000},
   {"recommended",  32000},
   {"productive",    8000},
   {"maintained",    4000},
   {"reviewed",      4000},
   {"conformant",    2000},
   {"compatible",    2000},
   {"coverage",      2000},
   {"specific",      1000},
                           
   {"unittest",      1000},
   {"shelltest",     1000},
   {"tested",         500},
   {"nodep",          250},
   {"libc",           250},
   {"configurable",    50},
   {"final",           50},
   {"global",           1},
   {"preview",        -50},
   {"memleak",       -250},
   {"experimental",  -500},
   {"difficult",     -500},
   {"unfinished",   -1000},
   {"old",          -1000},
   {"nodoc",        -1000},
   {"concept",      -2000},
   {"orphan",       -4000},
   {"obsolete",     -4000},
   {"discouraged", -32000},

	// clang-format on
};


int PluginDatabase::calculateStatus (std::string statusString)
{
	int ret = 0;
	std::istringstream ss (statusString);
	std::string status;
	while (ss >> status)
	{
		auto it = statusMap.find (status);
		if (it != statusMap.end ())
		{
			ret += it->second;
		}
		else
		{
			try
			{
				ret += stoi (status);
			}
			catch (std::invalid_argument)
			{
			}
		}
	}
	return ret;
}

PluginDatabase::Status ModulesPluginDatabase::status (PluginSpec const & spec) const
{
	PluginPtr plugin;
	try
	{
		KeySet conf = spec.getConfig ();
		conf.append (Key ("system/module", KEY_VALUE, "this plugin was loaded for the status", KEY_END));
		plugin = impl->modules.load (spec.getName (), conf);
		return real;
	}
	catch (...)
	{
		if (hasProvides (*this, spec.getName ()))
		{
			return provides;
		}
		else
		{
			return missing;
		}
	}
}

std::string ModulesPluginDatabase::lookupInfo (PluginSpec const & spec, std::string const & which) const
{
	PluginPtr plugin = impl->modules.load (spec.getName (), spec.getConfig ());
	return plugin->lookupInfo (which);
}

PluginDatabase::func_t ModulesPluginDatabase::getSymbol (PluginSpec const & spec, std::string const & which) const
{
	try
	{
		PluginPtr plugin = impl->modules.load (spec.getName (), spec.getConfig ());
		return plugin->getSymbol (which);
	}
	catch (...)
	{
		return NULL;
	}
}

PluginSpec ModulesPluginDatabase::lookupMetadata (std::string const & which) const
{
	std::vector<std::string> allPlugins = listAllPlugins ();
	std::map<int, PluginSpec> foundPlugins;

	std::string errors;
	// collect possible plugins
	for (auto const & plugin : allPlugins)
	{
		try
		{
			// TODO remove /module hack
			std::istringstream ss (
				lookupInfo (PluginSpec (plugin, KeySet (5, *Key ("system/module", KEY_VALUE,
										 "this plugin was loaded without a config", KEY_END),
									KS_END)),
					    "metadata"));
			std::string metadata;
			while (ss >> metadata)
			{
				if (metadata == which)
				{
					int s = calculateStatus (lookupInfo (
						PluginSpec (plugin, KeySet (5, *Key ("system/module", KEY_VALUE,
										     "this plugin was loaded without a config", KEY_END),
									    KS_END)),
						"status"));
					foundPlugins.insert (std::make_pair (s, PluginSpec (plugin)));
					break;
				}
			}
		}
		catch (std::exception const & e)
		{
			errors += e.what ();
			errors += ",";
		} // assume not loaded
	}

	if (foundPlugins.empty ())
	{
		if (!errors.empty ())
			throw NoPlugin ("No plugin that provides " + which + " could be found, got errors: " + errors);
		else
			throw NoPlugin ("No plugin that provides " + which + " could be found");
	}

	// the largest element of the map contains the best-suited plugin:
	return foundPlugins.rbegin ()->second;
}

PluginSpec ModulesPluginDatabase::lookupProvides (std::string const & which) const
{
	// check if plugin itself exists:
	if (status (PluginSpec (which)) == real)
	{
		return PluginSpec (which);
	}

	std::string errors;
	std::vector<std::string> allPlugins = listAllPlugins ();
	std::map<int, PluginSpec> foundPlugins;
	for (auto const & plugin : allPlugins)
	{
		// TODO: make sure (non)-equal plugins (i.e. with same/different contract) are handled correctly
		try
		{
			// TODO: support for generic plugins with config
			std::istringstream ss (
				lookupInfo (PluginSpec (plugin, KeySet (5, *Key ("system/module", KEY_VALUE,
										 "this plugin was loaded without a config", KEY_END),
									KS_END)),
					    "provides"));
			std::string provide;
			while (ss >> provide)
			{
				if (provide == which)
				{
					int s = calculateStatus (lookupInfo (
						PluginSpec (plugin, KeySet (5, *Key ("system/module", KEY_VALUE,
										     "this plugin was loaded without a config", KEY_END),
									    KS_END)),
						"status"));
					foundPlugins.insert (std::make_pair (s, PluginSpec (plugin)));
				}
			}
		}
		catch (std::exception const & e)
		{
			errors += e.what ();
			errors += ",";
		} // assume not loaded
	}

	if (foundPlugins.empty ())
	{
		if (!errors.empty ())
			throw NoPlugin ("No plugin that provides " + which + " could be found, got errors: " + errors);
		else
			throw NoPlugin ("No plugin that provides " + which + " could be found");
	}

	// the largest element of the map contains the best-suited plugin:
	return foundPlugins.rbegin ()->second;
}


std::vector<std::string> MockPluginDatabase::listAllPlugins () const
{
	std::vector<std::string> plugins;
	for (auto const & plugin : data)
	{
		plugins.push_back (plugin.first.getName ());
	}
	return plugins;
}

PluginDatabase::Status MockPluginDatabase::status (PluginSpec const & spec) const
{
	auto it = data.find (spec);
	if (it != data.end ())
	{
		return real;
	}

	if (hasProvides (*this, spec.getName ()))
	{
		return provides;
	}

	return missing;
}


std::string MockPluginDatabase::lookupInfo (PluginSpec const & spec, std::string const & which) const
{
	auto it = data.find (spec);
	if (it != data.end ())
	{
		return it->second[which];
	}

	return "";
}

/**
* @brief reference checkconf implementation for the unit tests.
* 
* Checks for the key "a" to be set to "abc" and fails if it can not find it.
* Checks for the key "b" and appends it to the configuration if it can not find it.
*/
static int checkconfMock (ckdb::Key * errorKey, ckdb::KeySet * conf)
{
	ckdb::Key * k;

	k = ckdb::ksLookupByName (conf, "user/a", 0);
	if (!k || strncmp (ckdb::keyString (k), "abc", 3))
	{
		return -1; // invalid config, could not fix
	}

	k = ckdb::ksLookupByName (conf, "user/b", 0);
	if (!k)
	{
		ckdb::ksAppendKey (conf, ckdb::keyNew ("user/b", 0));
		return 1; // changed config, OK
	}
	return 0; // did not change config, OK
}

PluginDatabase::func_t MockPluginDatabase::getSymbol (PluginSpec const & spec, std::string const & which) const
{
	if (which.find ("checkconf") != std::string::npos && spec.getFullName ().find ("checkconf") != std::string::npos)
	{
		return reinterpret_cast<func_t> (checkconfMock);
	}
	return NULL;
}
}
}

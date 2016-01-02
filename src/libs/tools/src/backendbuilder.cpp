/**
 * @file
 *
 * @brief Implementation of backend builder
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */



#include <backend.hpp>
#include <backends.hpp>
#include <backendbuilder.hpp>
#include <plugindatabase.hpp>


#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>
#include <helper/keyhelper.hpp>

#include <set>
#include <algorithm>

#include <kdb.hpp>
#include <cassert>


using namespace std;


namespace kdb
{


namespace tools
{


BackendBuilderInit::BackendBuilderInit() :
	pluginDatabase(make_shared<ModulesPluginDatabase>()),
	backendFactory("backend")
{
}


BackendBuilderInit::BackendBuilderInit(PluginDatabasePtr const & plugins) :
	pluginDatabase(plugins),
	backendFactory("backend")
{
}

BackendBuilderInit::BackendBuilderInit(BackendFactory const & bf) :
	pluginDatabase(make_shared<ModulesPluginDatabase>()),
	backendFactory(bf)
{
}

BackendBuilderInit::BackendBuilderInit(PluginDatabasePtr const & plugins, BackendFactory const & bf) :
	pluginDatabase(plugins),
	backendFactory(bf)
{
}

BackendBuilderInit::BackendBuilderInit(BackendFactory const & bf, PluginDatabasePtr const & plugins) :
	pluginDatabase(plugins),
	backendFactory(bf)
{
}


BackendBuilder::BackendBuilder(BackendBuilderInit const & bbi) :
	pluginDatabase(bbi.getPluginDatabase()),
	backendFactory(bbi.getBackendFactory())
{
}


BackendBuilder::~BackendBuilder()
{
}

MountBackendBuilder::MountBackendBuilder(BackendBuilderInit const & bbi) :
	BackendBuilder (bbi)
{
}

/**
 * @brief Parse a string containing information to create a KeySet
 *
 * @param pluginArguments comma (,) to separate key=value, contains no whitespaces
 *
 * @return newly created keyset with the information found in the string
 */
KeySet MountBackendBuilder::parsePluginArguments (std::string const & pluginArguments)
{
	KeySet ks;
	istringstream sstream(pluginArguments);

	std::string keyName;
	std::string value;

	// read until the next '=', this will be the keyname
	while (std::getline (sstream, keyName, '='))
	{
		// read until a ',' or the end of line
		// if nothing is read because the '=' is the last character
		// in the config string, consider the value empty
		if (!std::getline (sstream, value, ',')) value = "";

		ks.append (Key("user/"+keyName, KEY_VALUE, value.c_str(), KEY_END));
	}
	return ks;
}

/**
 * @brief Parse a complete commandline
 *
 * @param cmdline contains space separated plugins with optional plugin configurations
 *
 * @return a parsed PluginSpecVector
 */
PluginSpecVector MountBackendBuilder::parseArguments (std::string const & cmdline)
{
	// split cmdline
	PluginSpecVector arguments;
	std::string argument;
	istringstream sstream(cmdline);
	while (std::getline (sstream, argument, ' '))
	{
		if (argument.empty()) continue;
		if (std::all_of(argument.begin(), argument.end(),
			[](char c) {return std::isspace(c) || c==',';})) continue;
		if (argument.find ('=') == string::npos)
		{
			arguments.push_back(PluginSpec(argument));
		} else {
			if (arguments.empty()) throw ParseException("config for plugin ("+argument+") without previous plugin name");
			arguments.back().config = parsePluginArguments(argument);
		}
	}
	return arguments;
}

/**
 * @brief Makes sure that ordering constraints are fulfilled.
 *
 * @pre a sorted list except of the last element to be inserted
 * @post the last element will be moved to a place where it does not produce an order violation
 *
 * @note its still possible that an order violation is present in the case
 *       of order violation in the other direction (no cycle detection).
 */
void BackendBuilder::sort()
{
	auto hasOrderingConflict = [this] (PluginSpec const & other, PluginSpec const & inserted)
	{
		std::stringstream ss (pluginDatabase->lookupInfo(inserted, "ordering"));
		std::string order;
		while (ss >> order)
		{
			if (order == other.name)
			{
				return true;
			}

			if (order == pluginDatabase->lookupInfo(other, "provides"))
			{
				return true;
			}
		}
		return false;
	};

	// compare with all but the last
	for (auto i = toAdd.begin(); std::next(i) != toAdd.end(); ++i)
	{
		if (hasOrderingConflict(*i, toAdd.back()))
		{
			// bring last *before* the plugin where ordering constraint
			// would be violated
			PluginSpecVector n(toAdd.begin(), i);
			n.push_back(toAdd.back());
			n.insert(n.end(), i, std::prev(toAdd.end()));
			toAdd = n;
		}
	}
}

void BackendBuilder::resolveNeeds()
{
	// check if everything in toAdd is an actual plugin (and not virtual)
	for (auto & ps : toAdd)
	{
		try
		{
			PluginSpec toReplace = pluginDatabase->lookupProvides(ps.name);
			ps.name = toReplace.name;
			ps.config.append (toReplace.config);
		}
		catch (...)
		{
		}
	}

	std::vector<std::string> needs;

	do {
		// collect everything that is needed
		for (auto const & ps : toAdd)
		{
			std::stringstream ss (pluginDatabase->lookupInfo(ps, "needs"));
			std::string need;
			while (ss >> need)
			{
				needs.push_back(need);
			}
		}

		// remove what is already provided
		for (auto const & ps : toAdd)
		{
			std::string toRemove = ps.name;
			needs.erase(std::remove(needs.begin(), needs.end(), toRemove), needs.end());
			toRemove = pluginDatabase->lookupInfo(ps, "provides");
			needs.erase(std::remove(needs.begin(), needs.end(), toRemove), needs.end());
		}

		// leftover in needs is what is still needed
		for (auto const & need : needs)
		{
			addPlugin(pluginDatabase->lookupProvides(need));
			break; // only add one, it might resolve more than one need
		}
	} while (!needs.empty());
}

/**
 * @brief Add or update a plugin.
 *
 * Will automatically
 *
 * @param plugin
 */
void BackendBuilder::addPlugin (PluginSpec const & newPlugin)
{
	std::set<std::string> provides;
	{
		std::string providesString = pluginDatabase->lookupInfo(newPlugin, "provides");
		std::istringstream ss (providesString);
		std::string provide;
		while (ss >> provide)
		{
			provides.insert(provide);
		}
	}

	for (auto & p : toAdd)
	{
		if (p.name == newPlugin.name)
		{
			// newPlugin is already inserted:
			p.config.append(newPlugin.config);
			return;
		}

		if (provides.find(p.name) != provides.end())
		{
			// a plugin that provides newPlugin already is already inserted:
			p.name = newPlugin.name;
			p.config.append(newPlugin.config);
			return;
		}

		std::istringstream ss (pluginDatabase->lookupInfo(p, "provides"));
		std::string provide;
		while (ss >> provide)
		{
			if (newPlugin.name == provide)
			{
				// merge already existing concrete plugin with newly added provider
				p.config.append(newPlugin.config);
				return;
			}
		}
	}
	toAdd.push_back(newPlugin);
	sort();
}

void BackendBuilder::remPlugin (PluginSpec const & plugin)
{
	toAdd.erase(std::remove(toAdd.begin(), toAdd.end(), plugin));
}

void BackendBuilder::fillPlugins(BackendInterface & b) const
{
	for (auto const & plugin: toAdd)
	{
		b.addPlugin (plugin);
	}
}


void MountBackendBuilder::status (std::ostream & os) const
{
	try {
		MountBackendInterfacePtr b = getBackendFactory().create();
		fillPlugins(*b);
		return b->status (os);
	}
	catch (std::exception const & pce)
	{
		os << "Could not successfully add plugin: " << pce.what() << std::endl;
	}
}

bool MountBackendBuilder::validated () const
{
	try {
		MountBackendInterfacePtr b = getBackendFactory().create();
		fillPlugins(*b);
		return b->validated();
	}
	catch (...)
	{
		return false;
	}
}

void MountBackendBuilder::setMountpoint (Key mountpoint_, KeySet mountConf_)
{
	mountpoint = mountpoint_;
	mountConf = mountConf_;

	MountBackendInterfacePtr mbi = getBackendFactory().create();
	mbi->setMountpoint (mountpoint, mountConf);
}

std::string MountBackendBuilder::getMountpoint() const
{
	return mountpoint.getName();
}

void MountBackendBuilder::setBackendConfig (KeySet const & ks)
{
	backendConf = ks;
}

void MountBackendBuilder::useConfigFile (std::string file)
{
	configfile = file;

	MountBackendInterfacePtr b = getBackendFactory().create();
	bool checkPossible = false;
	for (auto const & p : *this)
	{
		if ("resolver" == getPluginDatabase()->lookupInfo(p, "provides"))
		{
			checkPossible = true;
		}
	}

	if (!checkPossible) return;
	fillPlugins (*b);
	b->useConfigFile (configfile);
}

std::string MountBackendBuilder::getConfigFile() const
{
	return configfile;
}

void MountBackendBuilder::serialize (kdb::KeySet &ret)
{
	MountBackendInterfacePtr mbi = getBackendFactory().create();
	fillPlugins (*mbi);
	mbi->setMountpoint (mountpoint, mountConf);
	mbi->setBackendConfig (backendConf);
	mbi->useConfigFile (configfile);
	mbi->serialize(ret);
}

}

}

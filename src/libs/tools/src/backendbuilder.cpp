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

#include <algorithm>

#include <kdb.hpp>
#include <cassert>


using namespace std;


namespace kdb
{


namespace tools
{


BackendBuilder::BackendBuilder() :
	pluginDatabase(make_shared<ModulesPluginDatabase>())
{
}


BackendBuilder::BackendBuilder(PluginDatabasePtr const & plugins) :
	pluginDatabase(plugins)
{
}

BackendBuilder::~BackendBuilder()
{
}

/**
 * @brief Parse a string containing information to create a KeySet
 *
 * @param pluginArguments comma (,) to separate key=value, contains no whitespaces
 *
 * @return newly created keyset with the information found in the string
 */
KeySet BackendBuilder::parsePluginArguments (std::string const & pluginArguments)
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
PluginSpecVector BackendBuilder::parseArguments (std::string const & cmdline)
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

void BackendBuilder::addPlugins (PluginSpecVector plugins)
{
	for (auto const & plugin : plugins)
	{
		addPlugin (plugin);
	}
}

void BackendBuilder::addPlugin (PluginSpec plugin)
{
	try {
		PluginSpec newPlugin = pluginDatabase->lookupProvides(plugin.name);
		plugin.name = newPlugin.name;
	} catch (...) {
	}
	toAdd.push_back(plugin);
	sort();
}

void BackendBuilder::remPlugin (PluginSpec plugin)
{
	toAdd.erase(std::remove(toAdd.begin(), toAdd.end(), plugin));
}

void BackendBuilder::status (std::ostream & os) const
{
	try {
		Backend b = create ();
		return b.status (os);
	}
	catch (std::exception const & pce)
	{
		os << "Could not successfully add plugin: " << pce.what() << std::endl;
	}
}

bool BackendBuilder::validated () const
{
	try {
		Backend b = create();
		return b.validated();
	}
	catch (...)
	{
		return false;
	}
}

Backend BackendBuilder::create() const
{
	Backend b;
	for (auto const & a: toAdd)
	{
		b.addPlugin(a.name, a.config);
	}
	return b;
}

void BackendBuilder::create(BackendInterface & b) const
{
	for (auto const & a: toAdd)
	{
		b.addPlugin(a.name, a.config);
	}
}

}

}

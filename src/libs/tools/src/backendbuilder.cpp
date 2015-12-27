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


BackendBuilder::BackendBuilder()
{
}


BackendBuilder::~BackendBuilder()
{
}

void BackendBuilder::parseArguments (std::string const & cmdline)
{
	// TODO: also parse plugins, not only conf
	KeySet pluginConfig;
	istringstream sstream(cmdline);

	std::string keyName;
	std::string value;

	// read until the next '=', this will be the keyname
	while (std::getline (sstream, keyName, '='))
	{
		// read until a ',' or the end of line
		// if nothing is read because the '=' is the last character
		// in the config string, consider the value empty
		if (!std::getline (sstream, value, ',')) value = "";

		pluginConfig.append(Key("user/"+keyName, KEY_VALUE, value.c_str(), KEY_END));
	}
}

/**
 * @brief Makes sure that ordering constraints are fulfilled.
 *
 * A stable sorting algorithm is used so that unaffected plugins
 * do not get mixed around.
 */
void BackendBuilder::sort()
{
	Modules modules;
	std::stable_sort(std::begin(toAdd), std::end(toAdd),
		[&modules] (PluginSpec const & lhs, PluginSpec const & rhs)
		{
			PluginPtr lhsPlugin = modules.load (lhs.name, lhs.config);
			PluginPtr rhsPlugin = modules.load (rhs.name, lhs.config);

			rhsPlugin->loadInfo();
			lhsPlugin->loadInfo();

			std::stringstream ss (lhsPlugin->lookupInfo("ordering"));
			std::string order;
			while (ss >> order)
			{
				if (order == rhsPlugin->name())
				{
					return true;
				}

				if (order == rhsPlugin->lookupInfo("provides"))
				{
					return true;
				}
			}
			return false;
		}
	    );
}

void BackendBuilder::addPlugin (PluginSpec plugin)
{
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
		Backend b = create();
		return b.status(os);
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

}

}

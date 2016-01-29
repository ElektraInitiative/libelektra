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
#include <pluginspec.hpp>
#include <backendparser.hpp>
#include <backendbuilder.hpp>
#include <plugindatabase.hpp>


#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>
#include <helper/keyhelper.hpp>

#include <set>
#include <algorithm>
#include <functional>

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
			if (order == other.getName())
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
			return;
		}
	}
}

/**
 * @brief resolve all needs that were not resolved by adding plugins.
 *
 * @see addPlugin()
 */
void BackendBuilder::resolveNeeds()
{
	// load dependency-plugins immediately
	for (auto const & ps : toAdd)
	{
		auto plugins = parseArguments (pluginDatabase->lookupInfo(ps, "plugins"));
		for (auto const & plugin : plugins)
		{
			addPlugin (plugin);
		}
	}

	std::vector<std::string> needs;
	do {
		needs.clear();

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

		for (auto const & ps : toAdd)
		{
			// remove the needed plugins that are already inserted
			needs.erase(std::remove(needs.begin(), needs.end(), ps.getName()), needs.end());

			// remove what is already provided
			std::string provides = pluginDatabase->lookupInfo(ps, "provides");
			std::istringstream ss (provides);
			std::string toRemove;
			while (ss >> toRemove)
			{
				needs.erase(std::remove(needs.begin(), needs.end(), toRemove), needs.end());
			}
		}

		// leftover in needs is what is still needed
		// will add one of them:
		if (!needs.empty())
		{
			addPlugin (PluginSpec(needs[0]));
			needs.erase(needs.begin());
		}
	} while (!needs.empty());
}

/**
 * @brief Add a plugin.
 *
 * @pre Needs to be a unique new name (use refname if you want to add the same module multiple times)
 *
 * Will automatically resolve virtual plugins to actual plugins.
 *
 * @see resolveNeeds()
 * @param plugin
 */
void BackendBuilder::addPlugin (PluginSpec const & plugin)
{
	for (auto & p : toAdd)
	{
		if (p.getFullName() == plugin.getFullName())
		{
			throw PluginAlreadyInserted();
		}
	}

	PluginSpec newPlugin = plugin;

	// if the plugin is actually a provider use it (otherwise we will get our name back):
	PluginSpec provides = pluginDatabase->lookupProvides (plugin.getName());
	if (provides.getName() != newPlugin.getName())
	{
		// keep our config and refname
		newPlugin.setName (provides.getName());
		newPlugin.appendConfig (provides.getConfig());
	}

	toAdd.push_back(newPlugin);
	sort();
}

void BackendBuilder::remPlugin (PluginSpec const & plugin)
{
	using namespace std::placeholders;
	PluginSpecFullName cmp;
	toAdd.erase(std::remove_if(toAdd.begin(), toAdd.end(), std::bind(cmp, plugin, _1) ));
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

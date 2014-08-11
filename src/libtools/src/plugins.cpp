/**
 * \file
 *
 * \brief Implementation of set/get/error plugins
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */




#include <plugins.hpp>

#include <kdbprivate.h>

#include <iterator>
#include <algorithm>

using namespace std;

namespace kdb
{

namespace tools
{


Plugins::Plugins () :
	plugins (NR_OF_PLUGINS),
	nrStoragePlugins (0),
	nrResolverPlugins (0)
{
	placementInfo["prerollback"] = Place(RESOLVER_PLUGIN, STORAGE_PLUGIN-1);
	placementInfo["rollback"] = Place(STORAGE_PLUGIN, STORAGE_PLUGIN);
	placementInfo["postrollback"] = Place(STORAGE_PLUGIN+1, NR_OF_PLUGINS-1);

	placementInfo["getresolver"] = Place(RESOLVER_PLUGIN, RESOLVER_PLUGIN);
	placementInfo["pregetstorage"] = Place(RESOLVER_PLUGIN+1, STORAGE_PLUGIN-1);
	placementInfo["getstorage"] = Place(STORAGE_PLUGIN, STORAGE_PLUGIN);
	placementInfo["postgetstorage"] = Place(STORAGE_PLUGIN+1, NR_OF_PLUGINS-1);
	revPostGet = NR_OF_PLUGINS-1;

	placementInfo["setresolver"] = Place(RESOLVER_PLUGIN, RESOLVER_PLUGIN);
	placementInfo["presetstorage"] = Place(RESOLVER_PLUGIN+1, STORAGE_PLUGIN-1);
	placementInfo["setstorage"] = Place(STORAGE_PLUGIN, STORAGE_PLUGIN);
	placementInfo["precommit"] = Place(STORAGE_PLUGIN+1, COMMIT_PLUGIN-1);
	placementInfo["commit"] = Place(COMMIT_PLUGIN, COMMIT_PLUGIN);
	placementInfo["postcommit"] = Place(COMMIT_PLUGIN+1, NR_OF_PLUGINS-1);
}

void Plugins::addInfo (Plugin &plugin)
{
	{
		std::string provide;
		std::stringstream ss(plugin.lookupInfo("provides"));
		while (ss >> provide)
		{
			alreadyProvided.push_back(provide);
		}
		/* Push back the name of the plugin itself */
		alreadyProvided.push_back (plugin.name());
	}

	{
		std::string need;
		std::stringstream ss(plugin.lookupInfo("needs"));
		while (ss >> need)
		{
			needed.push_back(need);
		}
	}

	{
		std::string recommend;
		std::stringstream ss(plugin.lookupInfo("recommends"));
		while (ss >> recommend)
		{
			recommended.push_back(recommend);
		}
	}

	{
		std::string conflict;
		std::stringstream ss(plugin.lookupInfo("conflicts"));
		while (ss >> conflict)
		{
			alreadyConflict.push_back(conflict);
		}
	}
}

void Plugins::addPlugin (Plugin &plugin, std::string which)
{
	if (!plugin.findInfo(which, "placements")) return;

	std::string stacking = plugin.lookupInfo("stacking");

	if (which=="postgetstorage" && stacking == "")
	{
		plugins[revPostGet --] = &plugin;
		return;
	}

	plugins[placementInfo[which].current++] = &plugin;
}

bool Plugins::checkPlacement (Plugin &plugin, std::string which)
{
	if (!plugin.findInfo(which, "placements")) return true;

	std::string stacking = plugin.lookupInfo("stacking");

	if (which=="postgetstorage" && stacking == "")
	{
		if (revPostGet >= placementInfo["postgetstorage"].current)
		{
			return true;
		}

		std::ostringstream os;
		os << "Too many plugins!\n"
			"The plugin can't be positioned anymore.\n"
			"Try to reduce the number of plugins!\n"
			"\n"
			"Failed because of stack overflow: cant place to "
			<< revPostGet  << " because "
			<< placementInfo["postgetstorage"].current
			<< " is larger (this slot is in use)." << endl;
		throw TooManyPlugins(os.str());
	}

	if (placementInfo[which].current > placementInfo[which].max)
	{
		std::ostringstream os;
		os << "Too many plugins!\n"
			"The plugin can't be positioned anymore.\n"
			"Try to reduce the number of plugins!\n"
			"\n"
			"Failed because " << which << " with "
			<< placementInfo[which].current << " is larger than "
			<< placementInfo[which].max << endl;
		throw TooManyPlugins(os.str());
	}

	return false;
}

bool Plugins::validateProvided() const
{
	return getNeededMissing().empty();
}

std::vector<std::string> Plugins::getNeededMissing() const
{
	std::vector<std::string> ret;
	for (size_t i=0; i< needed.size(); ++i)
	{
		std::string need = needed[i];
		if (std::find(alreadyProvided.begin(), alreadyProvided.end(), need) == alreadyProvided.end())
		{
			ret.push_back(need);
		}
	}
	return ret;
}

std::vector<std::string> Plugins::getRecommendedMissing() const
{
	std::vector<std::string> ret;
	for (size_t i=0; i< recommended.size(); ++i)
	{
		std::string recommend = recommended[i];
		if (std::find(alreadyProvided.begin(), alreadyProvided.end(), recommend) == alreadyProvided.end())
		{
			ret.push_back(recommend);
		}
	}
	return ret;
}

void Plugins::checkStorage (Plugin &plugin)
{
	if (plugin.findInfo("storage", "provides"))
	{
		++ nrStoragePlugins;
	}

	if (nrStoragePlugins>1)
	{
		-- nrStoragePlugins;
		throw StoragePlugin();
	}
}

void Plugins::checkResolver (Plugin &plugin)
{
	if (plugin.findInfo("resolver", "provides"))
	{
		++ nrResolverPlugins;
	}


	if (nrResolverPlugins>1)
	{
		-- nrResolverPlugins;
		throw ResolverPlugin();
	}

}


/** Check ordering of plugins.
  */
void Plugins::checkOrdering (Plugin &plugin)
{
	std::string order;
	std::stringstream ss(plugin.lookupInfo("ordering"));
	while (ss >> order)
	{
		/* Simple look in the already provided names.
		 * Because both plugin names + provided names are
		 * there.
		 * If it is found, we have an ordering violation.
		 */
		if (std::find(alreadyProvided.begin(), alreadyProvided.end(), order) != alreadyProvided.end())
		{
			throw OrderingViolation();
		}

	}
}

/** Check conflicts of plugins.
  */
void Plugins::checkConflicts (Plugin &plugin)
{
	{
		std::string order;
		std::stringstream ss(plugin.lookupInfo("conflicts"));
		while (ss >> order)
		{
			/* Simple look in the already provided names.
			 * Because both plugin names + provided names are
			 * there.
			 * If one is found, we have an conflict.
			 */
			if (std::find(alreadyProvided.begin(), alreadyProvided.end(), order) != alreadyProvided.end())
			{
				throw ConflictViolation();
			}
		}
	}

	/* Is there a conflict against the name? */
	if (std::find(alreadyConflict.begin(), alreadyConflict.end(), plugin.name()) != alreadyConflict.end())
	{
		throw ConflictViolation();
	}

	/* Is there a conflict against what it provides? */
	std::string order;
	std::stringstream ss(plugin.lookupInfo("provides"));
	while (ss >> order)
	{
		if (std::find(alreadyConflict.begin(), alreadyConflict.end(), order) != alreadyConflict.end())
		{
			throw ConflictViolation();
		}
	}
}






void ErrorPlugins::tryPlugin (Plugin &plugin)
{
	checkOrdering(plugin);
	checkConflicts(plugin);

	if (	checkPlacement(plugin,"prerollback") &&
		checkPlacement(plugin,"rollback") &&
		checkPlacement(plugin,"postrollback"))
	{
		/* Wont be added to errorplugins anyway, so ignore it */
		return;
	}

	if (!plugin.getSymbol("error"))
	{
		throw MissingSymbol("error");
	}

	checkResolver (plugin);
}


void GetPlugins::tryPlugin (Plugin &plugin)
{
	if (	checkPlacement(plugin, "getresolver") &&
		checkPlacement(plugin, "pregetstorage") &&
		checkPlacement(plugin, "getstorage") &&
		checkPlacement(plugin, "postgetstorage"))
	{
		/* Wont be added to errorplugins anyway, so ignore it */
		return;
	}

	if (!plugin.getSymbol("get"))
	{
		throw MissingSymbol("get");
	}

	checkStorage (plugin);
	checkResolver (plugin);
}

void SetPlugins::tryPlugin (Plugin &plugin)
{
	if (	checkPlacement(plugin, "setresolver") &&
		checkPlacement(plugin, "presetstorage") &&
		checkPlacement(plugin, "setstorage") &&
		checkPlacement(plugin, "precommit") &&
		checkPlacement(plugin, "commit") &&
		checkPlacement(plugin, "postcommit"))
	{
		/* Wont be added to errorplugins anyway, so ignore it */
		return;
	}

	if (!plugin.getSymbol("set"))
	{
		throw MissingSymbol("set");
	}


	checkStorage (plugin);
	checkResolver (plugin);
}






void ErrorPlugins::addPlugin (Plugin &plugin)
{
	Plugins::addPlugin (plugin, "prerollback");
	Plugins::addPlugin (plugin, "rollback");
	Plugins::addPlugin (plugin, "postrollback");

	Plugins::addInfo (plugin);
}

void GetPlugins::addPlugin (Plugin &plugin)
{
	Plugins::addPlugin (plugin, "getresolver");
	Plugins::addPlugin (plugin, "pregetstorage");
	Plugins::addPlugin (plugin, "getstorage");
	Plugins::addPlugin (plugin, "postgetstorage");
}

void SetPlugins::addPlugin (Plugin &plugin)
{
	Plugins::addPlugin (plugin, "setresolver");
	Plugins::addPlugin (plugin, "presetstorage");
	Plugins::addPlugin (plugin, "setstorage");
	Plugins::addPlugin (plugin, "precommit");
	Plugins::addPlugin (plugin, "commit");
	Plugins::addPlugin (plugin, "postcommit");
}


void ErrorPlugins::status (std::ostream & os) const
{
	std::vector<std::string> n= getNeededMissing();
	if (!n.empty())
	{
		os << "Needed plugins that are missing are: ";
		std::copy(n.begin(), n.end(),
			std::ostream_iterator<std::string>(os, " "));
		os << std::endl;
	}
	std::vector<std::string> r= getRecommendedMissing();
	if (!r.empty())
	{
		os << "Recommendations that are not fulfilled are: ";
		std::copy(r.begin(), r.end(),
			std::ostream_iterator<std::string>(os, " "));
		os << std::endl;
	}
}


bool ErrorPlugins::validated () const
{
	return nrResolverPlugins == 1 && validateProvided();
}

bool GetPlugins::validated () const
{
	return nrStoragePlugins == 1 && nrResolverPlugins == 1;
}

bool SetPlugins::validated () const
{
	return nrStoragePlugins == 1 && nrResolverPlugins == 1;
}





void ErrorPlugins::serialise (Key &baseKey, KeySet &ret)
{
	ret.append (*Key (baseKey.getName() + "/errorplugins",
		KEY_COMMENT, "List of plugins to use",
		KEY_END));

	for (int i=0; i< NR_OF_PLUGINS; ++i)
	{
		if (plugins[i] == 0) continue;

		std::ostringstream pluginNumber;
		pluginNumber << i;
		ret.append (*Key (baseKey.getName() + "/errorplugins/#" + pluginNumber.str() + plugins[i]->refname(),
			KEY_COMMENT, "A plugin",
			KEY_END));
	}
}

void GetPlugins::serialise (Key &baseKey, KeySet &ret)
{
	ret.append (*Key (baseKey.getName() + "/getplugins",
		KEY_COMMENT, "List of plugins to use",
		KEY_END));

	for (int i=0; i< NR_OF_PLUGINS; ++i)
	{
		if (plugins[i] == 0) continue;

		std::ostringstream pluginNumber;
		pluginNumber << i;
		ret.append (*Key (baseKey.getName() + "/getplugins/#" + pluginNumber.str() + plugins[i]->refname(),
			KEY_COMMENT, "A plugin",
			KEY_END));
	}
}


void SetPlugins::serialise (Key &baseKey, KeySet &ret)
{
	ret.append (*Key (baseKey.getName() + "/setplugins",
		KEY_COMMENT, "List of plugins to use",
		KEY_END));

	for (int i=0; i< NR_OF_PLUGINS; ++i)
	{
		if (plugins[i] == 0) continue;

		std::ostringstream pluginNumber;
		pluginNumber << i;
		ret.append (*Key (baseKey.getName() + "/setplugins/#" + pluginNumber.str() + plugins[i]->refname(),
			KEY_COMMENT, "A plugin",
			KEY_END));
	}
}

}

}

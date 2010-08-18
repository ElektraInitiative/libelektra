#include <plugins.hpp>

#include <kdbprivate.h>

#include <iostream>
#include <algorithm>

using namespace std;
using namespace kdb;


Plugins::Plugins () :
	plugins (10),
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

	placementInfo["setresolver"] = Place(RESOLVER_PLUGIN, RESOLVER_PLUGIN);
	placementInfo["presetstorage"] = Place(RESOLVER_PLUGIN+1, STORAGE_PLUGIN-1);
	placementInfo["setstorage"] = Place(STORAGE_PLUGIN, STORAGE_PLUGIN);
	placementInfo["precommit"] = Place(STORAGE_PLUGIN+1, COMMIT_PLUGIN-1);
	placementInfo["commit"] = Place(COMMIT_PLUGIN, COMMIT_PLUGIN);
	placementInfo["postcommit"] = Place(COMMIT_PLUGIN+1, NR_OF_PLUGINS-1);
}

void Plugins::addProvided (Plugin &plugin)
{
	{
		std::string provide;
		std::stringstream ss(plugin.lookupInfo("provides"));
		while (ss >> provide)
		{
			alreadyProvided.push_back(provide);
			cout << "add provide: " << provide << endl;
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
			cout << "add need: " << need << endl;
		}
	}

	{
		std::string recommend;
		std::stringstream ss(plugin.lookupInfo("recommends"));
		while (ss >> recommend)
		{
			recommended.push_back(recommend);
			cout << "add recommend: " << recommend << endl;
		}
	}
}

void Plugins::addPlugin (Plugin &plugin, std::string which)
{
	if (plugin.findInfo(which, "placements"))
	{
		cout << "Add plugin to " << placementInfo[which].current << endl;
		plugins[placementInfo[which].current++] = &plugin;
	}
}

bool Plugins::checkPlacement (Plugin &plugin, std::string which)
{
	if (!plugin.findInfo(which, "placements")) return true;

	if (placementInfo[which].current > placementInfo[which].max)
	{
		cout << "Failed because " << which << " with "
		     << placementInfo[which].current << " is larger than "
		     << placementInfo[which].max << endl;
		throw TooManyPlugins();
	}

	return false;
}

bool Plugins::validateProvided()
{
	for (size_t i=0; i< needed.size(); ++i)
	{
		std::string need = needed[i];
		if (std::find(alreadyProvided.begin(), alreadyProvided.end(), need) == alreadyProvided.end())
		{
			cout << "needed plugin " << need << " is missing" << endl;
			return false;
		}
	}

	for (size_t i=0; i< recommended.size(); ++i)
	{
		std::string need = recommended[i];
		if (std::find(alreadyProvided.begin(), alreadyProvided.end(), need) == alreadyProvided.end())
		{
			cout << "recommended plugin " << need << " is missing" << endl;
		}
	}

	return true;
}

void Plugins::checkStorage (Plugin &plugin)
{
	if (plugin.findInfo("storage", "provides"))
	{
		cout << "This is a storage plugin" << endl;
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
		cout << "This is a resolver plugin" << endl;
		++ nrResolverPlugins;
	}


	if (nrResolverPlugins>1)
	{
		-- nrResolverPlugins;
		throw ResolverPlugin();
	}

}

void Plugins::checkInfo (Plugin &plugin)
{
	if (!plugin.findInfo("BSD", "licence"))
	{
		cout << "Warning this plugin is not BSD licenced" << endl;
		cout << "It might taint the licence of the overall product" << endl;
		cout << "Its licence is: " << plugin.lookupInfo("licence") << endl;
	}
}






void ErrorPlugins::tryPlugin (Plugin &plugin)
{
	if (	checkPlacement(plugin,"prerollback") &&
		checkPlacement(plugin,"rollback") &&
		checkPlacement(plugin,"postrollback"))
	{
		/* Wont be added to errorplugins anyway, so ignore it */
		cout << "Wont be in error plugin, omitting tests" << endl;
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
		cout << "Wont be in get plugin, omitting tests" << endl;
		return;
	}

	if (!plugin.getSymbol("get"))
	{
		throw MissingSymbol("get");
	}

	checkStorage (plugin);
	checkResolver (plugin);
	checkInfo (plugin);
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
		cout << "Wont be in set plugin, omitting tests" << endl;
		return;
	}

	if (!plugin.getSymbol("set"))
	{
		throw MissingSymbol("set");
	}


	checkStorage (plugin);
	checkResolver (plugin);
	checkInfo (plugin);
}






void ErrorPlugins::addPlugin (Plugin &plugin)
{
	Plugins::addPlugin (plugin, "prerollback");
	Plugins::addPlugin (plugin, "rollback");
	Plugins::addPlugin (plugin, "postrollback");

	Plugins::addProvided (plugin);
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




bool ErrorPlugins::validated ()
{
	return nrResolverPlugins == 1 && validateProvided();
}

bool GetPlugins::validated ()
{
	return nrStoragePlugins == 1 && nrResolverPlugins == 1;
}

bool SetPlugins::validated ()
{
	return nrStoragePlugins == 1 && nrResolverPlugins == 1;
}





void ErrorPlugins::serialize (Key &baseKey, KeySet &ret)
{
	ret.append (*Key (baseKey.getName() + "/errorplugins",
		KEY_COMMENT, "List of plugins to use",
		KEY_END));

	for (int i=0; i< 10; ++i)
	{
		if (plugins[i] == 0) continue;

		std::ostringstream pluginNumber;
		pluginNumber << i;
		ret.append (*Key (baseKey.getName() + "/errorplugins/#" + pluginNumber.str() + plugins[i]->refname(),
			KEY_COMMENT, "A plugin",
			KEY_END));
	}
}

void GetPlugins::serialize (Key &baseKey, KeySet &ret)
{
	ret.append (*Key (baseKey.getName() + "/getplugins",
		KEY_COMMENT, "List of plugins to use",
		KEY_END));

	for (int i=0; i< 10; ++i)
	{
		if (plugins[i] == 0) continue;

		std::ostringstream pluginNumber;
		pluginNumber << i;
		ret.append (*Key (baseKey.getName() + "/getplugins/#" + pluginNumber.str() + plugins[i]->refname(),
			KEY_COMMENT, "A plugin",
			KEY_END));
	}
}


void SetPlugins::serialize (Key &baseKey, KeySet &ret)
{
	ret.append (*Key (baseKey.getName() + "/setplugins",
		KEY_COMMENT, "List of plugins to use",
		KEY_END));

	for (int i=0; i< 10; ++i)
	{
		if (plugins[i] == 0) continue;

		std::ostringstream pluginNumber;
		pluginNumber << i;
		ret.append (*Key (baseKey.getName() + "/setplugins/#" + pluginNumber.str() + plugins[i]->refname(),
			KEY_COMMENT, "A plugin",
			KEY_END));
	}
}

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
	placementInfo["precommit"] = Place(STORAGE_PLUGIN+1, COMMIT_PLUGIN);
	placementInfo["commit"] = Place(COMMIT_PLUGIN, COMMIT_PLUGIN);
	placementInfo["postcommit"] = Place(COMMIT_PLUGIN, NR_OF_PLUGINS-1);
}

void Plugins::addProvided (Plugin &plugin)
{
	std::string provide;
	std::stringstream ss(plugin.lookupInfo("provides"));
	while (ss >> provide)
	{
		alreadyProvided.push_back(provide);
		cout << "add provide: " << provide << endl;
	}
}

bool Plugins::checkPlacement (Plugin &plugin, std::string which)
{
	std::string placement = plugin.lookupInfo("placements");
	if (placement.find(which) == string::npos) return true;

	if (placementInfo[which].current > placementInfo[which].max)
	{
		throw TooManyPlugins();
	}

	return false;
}

void Plugins::checkProvided(Plugin &plugin)
{
	std::string need;
	std::stringstream nss(plugin.lookupInfo("needs"));
	while (nss >> need)
	{
		cout << "check for need " << need << endl;;
		if (std::find(alreadyProvided.begin(), alreadyProvided.end(), need) == alreadyProvided.end())
		{
			throw MissingNeeded(need);
		}
	}

}

void Plugins::checkStorage (Plugin &plugin)
{
	if (std::string(plugin.lookupInfo("provides")).find("storage") != string::npos)
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
	if (std::string(plugin.lookupInfo("provides")).find("resolver") != string::npos)
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
	if (std::string(plugin.lookupInfo("licence")).find("BSD") == string::npos)
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
	if (std::string(plugin.lookupInfo("placements")).find("prerollback") != string::npos)
	{
		plugins[placementInfo["prerollback"].current++] = &plugin;
	}

	if (std::string(plugin.lookupInfo("placements")).find("rollback") != string::npos)
	{
		plugins[placementInfo["rollback"].current++] = &plugin;
	}

	if (std::string(plugin.lookupInfo("placements")).find("postrollback") != string::npos)
	{
		plugins[placementInfo["postrollback"].current++] = &plugin;
	}
}

void GetPlugins::addPlugin (Plugin &plugin)
{
	if (std::string(plugin.lookupInfo("placements")).find("pregetstorage") != string::npos)
	{
		cout << "Add plugin to " << placementInfo["pregetstorage"].current << endl;
		plugins[placementInfo["pregetstorage"].current++] = &plugin;
	}

	if (std::string(plugin.lookupInfo("placements")).find("postgetstorage") != string::npos)
	{
		cout << "Add plugin to " << placementInfo["postgetstorage"].current << endl;
		plugins[placementInfo["postgetstorage"].current++] = &plugin;
	}

	if (std::string(plugin.lookupInfo("provides")).find("storage") != string::npos)
	{
		// hack, do with proper placement
		plugins[3] = &plugin;
	}

	if (std::string(plugin.lookupInfo("provides")).find("resolver") != string::npos)
	{
		// hack, do with proper placement
		plugins[0] = &plugin;
	}
}

void SetPlugins::addPlugin (Plugin &plugin)
{
	if (std::string(plugin.lookupInfo("placements")).find("presetstorage") != string::npos)
	{
		cout << "Add plugin to " << placementInfo["presetstorage"].current << endl;
		plugins[placementInfo["presetstorage"].current++] = &plugin;
	}

	if (std::string(plugin.lookupInfo("provides")).find("storage") != string::npos)
	{
		// hack, do with proper placement
		plugins[3] = &plugin;
	}

	if (std::string(plugin.lookupInfo("provides")).find("resolver") != string::npos)
	{
		// hack, do with proper placement
		plugins[0] = &plugin;
		plugins[7] = &plugin;
	}
}




bool ErrorPlugins::validated ()
{
	return nrResolverPlugins == 1;
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

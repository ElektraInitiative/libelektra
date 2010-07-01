#include <plugins.hpp>

#include <iostream>
#include <algorithm>

using namespace std;
using namespace kdb;

bool Plugins::checkStorage (Plugin &plugin)
{
	bool isStoragePlugin = false;

	std::string provide;
	std::stringstream ss(plugin.lookupInfo("provides"));
	while (ss >> provide)
	{
		alreadyProvided.push_back(provide);
		cout << "add provide: " << provide << endl;
	}

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

	if (std::string(plugin.lookupInfo("provides")).find("storage") != string::npos)
	{
		cout << "This is a storage plugin" << endl;
		++ nrStoragePlugins;
		isStoragePlugin = true;
	}

	if (nrStoragePlugins>1)
	{
		throw StoragePlugin();
	}

	if (nrStoragePlugins == 0)
	{
		cerr << "You need to provide a storage plugin, but did not" << endl;
		return false;
	}

	return true;
}

bool Plugins::checkInfo (Plugin &plugin)
{
	if (std::string(plugin.lookupInfo("licence")).find("BSD") == string::npos)
	{
		cout << "Warning this plugin is not BSD licenced" << endl;
		cout << "It might taint the licence of the overall product" << endl;
		cout << "Its licence is: " << plugin.lookupInfo("licence") << endl;
	}

	return true;
}




bool ErrorPlugins::addPlugin (Plugin &plugin)
{
	bool ret = true;

	if (std::string(plugin.lookupInfo("provides")).find("storage") != string::npos)
	{
		cout << "Ignore storage plugin in ErrorPlugins" << endl;
		return ret;
	}

	if (!plugin.getSymbol("error"))
	{
		throw MissingSymbol("error");
	}

	if (!checkInfo (plugin)) ret = false;

	if (std::string(plugin.lookupInfo("provides")).find("resolver") != string::npos)
	{
		// hack, do with proper placement
		plugins[0] = &plugin;
	}

	return ret;
}


bool GetPlugins::addPlugin (Plugin &plugin)
{
	bool ret = true;


	cout << "Will add a plugin" << endl;

	if (!plugin.getSymbol("get"))
	{
		throw MissingSymbol("get");
	}

	if (!checkStorage (plugin)) ret = false;
	if (!checkInfo (plugin)) ret = false;

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


	return true;
}

bool SetPlugins::addPlugin (Plugin &plugin)
{
	bool ret = true;

	if (!plugin.getSymbol("set"))
	{
		throw MissingSymbol("set");
	}


	if (!checkStorage (plugin)) ret = false;
	if (!checkInfo (plugin)) ret = false;

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

	return ret;
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

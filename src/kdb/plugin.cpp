#include <kdb>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbmodule.h>

#include <kdbprivate.h>

#include <plugin.hpp>

using namespace std;
using namespace kdb;

Plugin::Plugin(std::string const& pluginName, KeySet &modules, KeySet const& testConfig) :
	pluginName(pluginName)
{
	Key errorKey;
	plugin = ckdb::elektraPluginOpen(pluginName.c_str(), modules.getKeySet(), testConfig.dup(), *errorKey);

	if (!plugin)
	{
		printError(errorKey);
		printWarnings(errorKey);
		throw NoPlugin();
	}

	Key infoKey ("system/elektra/modules", KEY_END);
	infoKey.addBaseName(pluginName);

	if (!plugin->kdbGet)
	{
		close();
		throw MissingSymbol("kdbGet");
	}
	plugin->kdbGet(plugin, info.getKeySet(), *infoKey);
}

Plugin::~Plugin()
{
	close();
}

void Plugin::close()
{
	Key errorKey;
	ckdb::elektraPluginClose(plugin, *errorKey);
	printWarnings(errorKey);
}

ckdb::Plugin *Plugin::operator->()
{
	return plugin;
}

bool Plugin::operator!()
{
	return !plugin;
}

std::string Plugin::lookupInfo(std::string item, std::string section)
{
	Key k ("system/elektra/modules", KEY_END);
	k.addBaseName(pluginName);
	k.addBaseName(section);
	k.addBaseName(item);
	Key ret = info.lookup(k);

	if (!ret) return ""; /* Lets say missing info is ok for now */

	return ret.getString();
}

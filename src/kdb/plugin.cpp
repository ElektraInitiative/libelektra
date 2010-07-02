#include <kdb>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbmodule.h>

#include <kdbprivate.h>

#include <plugin.hpp>

using namespace std;
using namespace kdb;

Plugin::Plugin(std::string const& pluginName, KeySet &modules, KeySet const& testConfig) :
	pluginName(pluginName),
	firstRef (true)
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

	parse();
}

Plugin::Plugin(Plugin const& other) :
	plugin(other.plugin),
	pluginName(other.pluginName),
	info(other.info),
	symbols(other.symbols),
	infos(other.infos),
	firstRef(other.firstRef)
{
	++plugin->refcounter;
}

Plugin& Plugin::operator = (Plugin const& other)
{
	if (this == &other) return *this;

	close();

	plugin = other.plugin;
	pluginName = other.pluginName;
	info = other.info;
	symbols = other.symbols;
	infos = other.infos;
	firstRef = other.firstRef;

	++plugin->refcounter;

	return *this;
}

Plugin::~Plugin()
{
	close();
}

void Plugin::parse ()
{
	Key root (std::string("system/elektra/modules/") + pluginName + "/exports", KEY_END);
	Key k = info.lookup (root);

	if (k)
	{
		while ((k = info.next()) && k.getDirName() == root.getName())
		{
			symbols[k.baseName()] = (*(func_t*) k.value());
		}
	}
	if (plugin->kdbGet) symbols["open"] = (func_t) plugin->kdbOpen;
	if (plugin->kdbGet) symbols["close"] = (func_t) plugin->kdbClose;
	if (plugin->kdbGet) symbols["get"] = (func_t) plugin->kdbGet;
	if (plugin->kdbGet) symbols["set"] = (func_t) plugin->kdbSet;
	if (plugin->kdbGet) symbols["error"] = (func_t) plugin->kdbError;

	root.setName(std::string("system/elektra/modules/") + pluginName + "/infos");
	k = info.lookup (root);

	if (k)
	{
		while ((k = info.next()) && k.getDirName() == root.getName())
		{
			infos[k.baseName()] = k.getString();
		}
	}

}

void Plugin::close()
{
	/* ref counting will avoid closing */

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

std::string Plugin::name()
{
	return pluginName;
}

std::string Plugin::refname()
{
	if (firstRef)
	{
		firstRef = false;
		return std::string("#") + pluginName + "#" + pluginName + "#";
	} else {
		return std::string("#") + pluginName;
	}
}

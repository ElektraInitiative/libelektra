/**
 * @file
 *
 * @brief Implementation of plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <kdb.hpp>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbmodule.h>
#include <kdbprivate.h> // currently needed for plugin handling
#include <helper/keyhelper.hpp>

#include <set>
#include <algorithm>

#include <plugin.hpp>

// for stdout
#include <stdio.h>

using namespace std;

namespace kdb
{

namespace tools
{

Plugin::Plugin(std::string  nameOfNewPlugin, KeySet &modules, KeySet const& pluginConfig) :
	pluginName(std::move(nameOfNewPlugin)),
	firstRef (true)
{
	Key errorKey;
	plugin = ckdb::elektraPluginOpen(pluginName.c_str(), modules.getKeySet(), pluginConfig.dup(), *errorKey);

	if (!plugin)
	{
		throw NoPlugin(errorKey);
	}
}

kdb::KeySet Plugin::getConfig()
{
	return ksDup(elektraPluginGetConfig(plugin));
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

	uninit();

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
	uninit();
}

void Plugin::uninit()
{
	/* ref counting will avoid closing */

	Key errorKey;
	ckdb::elektraPluginClose(plugin, errorKey.getKey());
}

void Plugin::loadInfo()
{
	Key infoKey ("system/elektra/modules", KEY_END);
	infoKey.addBaseName(pluginName);

	if (pluginName != plugin->name)
	{
		throw PluginWrongName();
	}

	if (!plugin->kdbGet)
	{
		throw MissingSymbol("kdbGet");
	}
	plugin->kdbGet(plugin, info.getKeySet(), *infoKey);
}

void Plugin::parse ()
{
	Key root (std::string("system/elektra/modules/") + pluginName, KEY_END);

	Key k = info.lookup (root);
	if (!k)
	{
		throw PluginNoContract();
	}

	root.setName(std::string("system/elektra/modules/") + pluginName + "/exports");

	k = info.lookup (root);

	if (k)
	{
		while ((k = info.next()) && k.isBelow(root))
		{
			symbols[k.getBaseName()] = (*k.getFunc());
		}
	}

	root.setName(std::string("system/elektra/modules/") + pluginName + "/infos");
	k = info.lookup (root);

	if (k)
	{
		while ((k = info.next()) && k.isBelow(root))
		{
			infos[k.getBaseName()] = k.getString();
		}
	} else {
		throw PluginNoInfo();
	}

}

void Plugin::check(vector<string> & warnings)
{
	if (infos.find("version") == infos.end()) warnings.push_back ("no version found");
	else if (infos["version"] != PLUGINVERSION) throw VersionInfoMismatch();

	if (infos.find("licence") == infos.end()) warnings.push_back ("no licence information found");
	else if (infos["licence"] != "BSD") warnings.push_back
		("the licence is not BSD, it might change the overall licence of your elektra installation");

	if (infos.find("description") == infos.end()) warnings.push_back ("no description of the plugin found");

	if (infos.find("provides") == infos.end()) warnings.push_back ("no provides information found");
	if (infos.find("placements") == infos.end())
	{
		warnings.push_back ("no placements information found");
	} else {
		std::vector<std::string> pp;
		pp.push_back("prerollback");
		pp.push_back("rollback");
		pp.push_back("postrollback");
		pp.push_back("getresolver");
		pp.push_back("pregetstorage");
		pp.push_back("getstorage");
		pp.push_back("postgetstorage");
		pp.push_back("setresolver");
		pp.push_back("presetstorage");
		pp.push_back("setstorage");
		pp.push_back("precommit");
		pp.push_back("commit");
		pp.push_back("postcommit");
		std::string placements = infos["placements"];
		istringstream is(placements);
		std::string placement;
		while (is >> placement)
		{
			if (std::find(pp.begin(), pp.end(), placement) == pp.end())
			{
				warnings.push_back ("not supported placement "
					+ placement
					+ " found");
			}
		}
	}
	if (infos.find("needs") == infos.end()) warnings.push_back ("no needs information found");

	if (infos.find("author") == infos.end())
	{
		warnings.push_back ("no author found");
	} else {
		std::string author = infos["author"];
		size_t ppos = 0;
		ppos = author.find ('<', ppos);
		if (ppos == string::npos) warnings.push_back ("Could not find \"<\" for authors e-mail address");

		size_t pos = 0;
		pos = author.find ('@', ppos);
		if (pos == string::npos) warnings.push_back ("Could not find \"@\" for authors e-mail address");
		if (pos < ppos) warnings.push_back ("@ found before <");

		size_t lpos = 0;
		lpos = author.find ('>', pos);
		if (lpos == string::npos) warnings.push_back ("Could not find \">\" for authors e-mail address");
		if (lpos < pos) warnings.push_back ("> found before @");
	}

	std::set<func_t> checkDups;
	std::pair<std::set<func_t>::iterator, bool> ret;
	if (plugin->kdbOpen)
	{
		if (symbols.find("open") == symbols.end()) warnings.push_back ("no open symbol exported");
		else if (symbols["open"] != reinterpret_cast<func_t>(plugin->kdbOpen)) throw SymbolMismatch ("open");
		ret = checkDups.insert(symbols["open"]);
		if (!ret.second) throw SymbolDuplicate("open");
	}
	if (plugin->kdbClose)
	{
		if (symbols.find("close") == symbols.end()) warnings.push_back ("no close symbol exported");
		else if (symbols["close"] != reinterpret_cast<func_t>(plugin->kdbClose)) throw SymbolMismatch ("close");
		ret = checkDups.insert(symbols["close"]);
		if (!ret.second) throw SymbolDuplicate("close");
	}
	if (plugin->kdbGet)
	{
		if (symbols.find("get") == symbols.end()) warnings.push_back ("no get symbol exported");
		else if (symbols["get"] != reinterpret_cast<func_t>(plugin->kdbGet)) throw SymbolMismatch ("get");
		ret = checkDups.insert(symbols["get"]);
		if (!ret.second) throw SymbolDuplicate("get");
	}
	if (plugin->kdbSet)
	{
		if (symbols.find("set") == symbols.end()) warnings.push_back ("no set symbol exported");
		else if (symbols["set"] != reinterpret_cast<func_t>(plugin->kdbSet)) throw SymbolMismatch ("set");
		ret = checkDups.insert(symbols["set"]);
		if (!ret.second) throw SymbolDuplicate("set");
	}
	if (plugin->kdbError)
	{
		if (symbols.find("error") == symbols.end()) warnings.push_back ("no error symbol exported");
		else if (symbols["error"] != reinterpret_cast<func_t>(plugin->kdbError)) throw SymbolMismatch ("error");
		ret = checkDups.insert(symbols["error"]);
		if (!ret.second) throw SymbolDuplicate("error");
	}
	if (symbols.find("open") != symbols.end())
	{
		if (!plugin->kdbOpen) throw SymbolMismatch ("open");
	}
	if (symbols.find("close") != symbols.end())
	{
		if (!plugin->kdbClose) throw SymbolMismatch ("close");
	}
	if (symbols.find("get") != symbols.end())
	{
		if (!plugin->kdbGet) throw SymbolMismatch ("get");
	}
	if (symbols.find("set") != symbols.end())
	{
		if (!plugin->kdbSet) throw SymbolMismatch ("set");
	}
	if (symbols.find("error") != symbols.end())
	{
		if (!plugin->kdbError) throw SymbolMismatch ("error");
	}
}

ckdb::Plugin *Plugin::operator->()
{
	return plugin;
}

std::string Plugin::lookupInfo(std::string item, std::string section)
{
	Key k ("system/elektra/modules", KEY_END);
	k.addBaseName(pluginName);
	k.addBaseName(section);
	k.addBaseName(item);
	Key ret = info.lookup(k);

	if (!ret) return ""; /* TODO Lets say missing info is ok for now */

	return ret.getString();
}

bool Plugin::findInfo(std::string compare, std::string item, std::string section)
{
	std::string str = lookupInfo (item, section);

	std::istringstream istr (str);

	std::string toCheck;
	while (istr >> toCheck)
	{
		if (toCheck == compare) return true;
	}
	return false;
}

kdb::KeySet Plugin::getNeededConfig()
{
	Key neededConfigKey ("system/elektra/modules", KEY_END);
	neededConfigKey.addName(pluginName);
	neededConfigKey.addName("config/needs");

	KeySet d (info.dup());
	KeySet config = d.cut(neededConfigKey);

	KeySet ret;
	Key oldParent = neededConfigKey;
	Key newParent("system", KEY_END);
	for (KeySet::iterator i = config.begin(); i != config.end(); ++i)
	{
		Key k(i->dup());
		ret.append(kdb::tools::helper::rebaseKey(k, oldParent, newParent));
	}
	return ret;
}

int Plugin::open (kdb::Key & errorKey)
{
	if (!plugin->kdbOpen)
	{
		throw MissingSymbol("kdbOpen");
	}

	return plugin->kdbOpen(plugin, errorKey.getKey());
}

int Plugin::close (kdb::Key & errorKey)
{
	if (!plugin->kdbClose)
	{
		throw MissingSymbol("kdbClose");
	}

	return plugin->kdbClose(plugin, errorKey.getKey());
}

int Plugin::get (kdb::KeySet & ks, kdb::Key & parentKey)
{
	if (!plugin->kdbGet)
	{
		throw MissingSymbol("kdbGet");
	}

	return plugin->kdbGet(plugin, ks.getKeySet(), parentKey.getKey());
}

int Plugin::set (kdb::KeySet & ks, kdb::Key & parentKey)
{
	if (!plugin->kdbSet)
	{
		throw MissingSymbol("kdbSet");
	}

	return plugin->kdbSet(plugin, ks.getKeySet(), parentKey.getKey());
}

int Plugin::error (kdb::KeySet & ks, kdb::Key & parentKey)
{
	if (!plugin->kdbError)
	{
		throw MissingSymbol("kdbError");
	}

	return plugin->kdbError(plugin, ks.getKeySet(), parentKey.getKey());
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

}

}

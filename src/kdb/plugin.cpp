#include <kdb.hpp>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbmodule.h>
#include <kdbprivate.h>

#include <iostream>

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

void Plugin::loadInfo()
{
	Key infoKey ("system/elektra/modules", KEY_END);
	infoKey.addBaseName(pluginName);

	if (pluginName != plugin->name)
	{
		uninit();
		throw PluginWrongName();
	}

	if (!plugin->kdbGet)
	{
		uninit();
		throw MissingSymbol("kdbGet");
	}
	plugin->kdbGet(plugin, info.getKeySet(), *infoKey);
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

	root.setName(std::string("system/elektra/modules/") + pluginName + "/infos");
	k = info.lookup (root);

	if (k)
	{
		while ((k = info.next()) && k.getDirName() == root.getName())
		{
			infos[k.baseName()] = k.getString();
		}
	} else {
		throw PluginNoInfo();
	}

}

void Plugin::check(vector<string> & warnings)
{
	if (infos.find("licence") == infos.end()) warnings.push_back ("no licence information found");
	else if (infos["licence"] != "BSD") warnings.push_back
		("the licence is not BSD, it might change the overall licence of your elektra installation");


	if (infos.find("description") == infos.end()) warnings.push_back ("no description of the plugin found");

	if (infos.find("provides") == infos.end()) warnings.push_back ("no provides information found");
	if (infos.find("placements") == infos.end()) warnings.push_back ("no placements information found");
	if (infos.find("needs") == infos.end()) warnings.push_back ("no needs information found");

	if (infos.find("version") == infos.end()) warnings.push_back ("no version found");
	else if (infos["version"] != PLUGINVERSION) throw VersionInfoMismatch();

	if (infos.find("author") == infos.end()) warnings.push_back ("no author found");
	else {
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

	if (plugin->kdbOpen)
	{
		if (symbols.find("open") == symbols.end()) warnings.push_back ("no open symbol exported");
		else if (symbols["open"] != (func_t) plugin->kdbOpen) throw SymbolMismatch ("open");
	}
	if (plugin->kdbClose)
	{
		if (symbols.find("close") == symbols.end()) warnings.push_back ("no close symbol exported");
		else if (symbols["close"] != (func_t) plugin->kdbClose) throw SymbolMismatch ("close");
	}
	if (plugin->kdbGet)
	{
		if (symbols.find("get") == symbols.end()) warnings.push_back ("no get symbol exported");
		else if (symbols["get"] != (func_t) plugin->kdbGet) throw SymbolMismatch ("get");
	}
	if (plugin->kdbSet)
	{
		if (symbols.find("set") == symbols.end()) warnings.push_back ("no set symbol exported");
		else if (symbols["set"] != (func_t) plugin->kdbSet) throw SymbolMismatch ("set");
	}
	if (plugin->kdbError)
	{
		if (symbols.find("error") == symbols.end()) warnings.push_back ("no error symbol exported");
		else if (symbols["error"] != (func_t) plugin->kdbError) throw SymbolMismatch ("error");
	}
}

void Plugin::uninit()
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

bool Plugin::findInfo(std::string check, std::string item, std::string section)
{
	std::string str = lookupInfo (item, section);

	std::istringstream istr (str);

	std::string toCheck;
	while (istr >> toCheck)
	{
		if (toCheck == check) return true;
	}
	return false;
}

kdb::KeySet Plugin::getNeededConfig()
{
	Key neededConfigKey ("system/elektra/modules", KEY_END);
	neededConfigKey.addBaseName(pluginName);
	neededConfigKey.addBaseName("config/needs");

	KeySet d (info.dup());
	return d.cut(neededConfigKey);
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


void Plugin::serialize (KeySet & ks)
{
	try {
		typedef int  (*serialize_t)(std::ostream &, ckdb::Key *, ckdb::KeySet *);
		Plugin::func_t fun = getSymbol ("serialize");
		serialize_t ser_fun = reinterpret_cast<serialize_t> (fun);

		ser_fun(std::cout, 0, ks.getKeySet());
		return;
	} catch (...) {}

	try {
		typedef void (*serialize_t)(std::ostream& os, kdb::KeySet & output);
		Plugin::func_t fun = getSymbol ("cpp_serialize");
		serialize_t ser_fun = reinterpret_cast<serialize_t> (fun);

		ser_fun(std::cout, ks);
		return;
	} catch (...) {}

	try {
		typedef ssize_t (*serialize_t)(const ckdb::KeySet *ks, FILE* stream, option_t options);
		Plugin::func_t fun = getSymbol ("ksToStream");
		serialize_t ser_fun = reinterpret_cast<serialize_t> (fun);

		ser_fun(ks.getKeySet(), stdout, 0);
		return;
	} catch (...) {}
}

void Plugin::unserialize (KeySet & ks)
{
	try {
		typedef int (*unserialize_t)(std::istream &is, ckdb::Key *errorKey, ckdb::KeySet *ks);
		Plugin::func_t fun = getSymbol ("unserialize");
		unserialize_t ser_fun = reinterpret_cast<unserialize_t> (fun);

		ser_fun(cin, 0, ks.getKeySet());
		return;
	} catch (...) {}

	try {
		typedef void (*unserialize_t)(std::istream& os, kdb::KeySet & output);
		Plugin::func_t fun = getSymbol ("cpp_unserialize");
		unserialize_t ser_fun = reinterpret_cast<unserialize_t> (fun);

		ser_fun(cin, ks);
		return;
	} catch (...) {}

	try {
		typedef int (*unserialize_t)(ckdb::KeySet *ks, int fd);
		Plugin::func_t fun = getSymbol ("ksFromXML");
		unserialize_t ser_fun = reinterpret_cast<unserialize_t> (fun);

		ser_fun(ks.getKeySet(), 0); // read from stdin
		return;
	} catch (...) {}
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

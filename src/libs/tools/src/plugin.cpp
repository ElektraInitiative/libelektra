/**
 * @file
 *
 * @brief Implementation of plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <kdb.hpp>

#include <helper/keyhelper.hpp>
#include <kdb.h>
#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h> // currently needed for plugin handling (struct _Plugin)
#include <plugindatabase.hpp>

#include <algorithm>
#include <set>

#include <plugin.hpp>

// for stdout
#include <stdio.h>

using namespace std;

namespace kdb
{

namespace tools
{

Plugin::Plugin (PluginSpec const & spec_, KeySet & modules) : spec (spec_), firstRef (true)
{
	Key errorKey;
	plugin = ckdb::elektraPluginOpen (spec.getName ().c_str (), modules.getKeySet (), spec.getConfig ().dup (), *errorKey);

	if (!plugin)
	{
		throw NoPlugin (errorKey);
	}

	// plugin->name might be different for default plugins:
	if (spec.getName () != plugin->name)
	{
		spec.setRefName (spec.getName ()); // save virtual name as refname
		spec.setName (plugin->name);	   // use actual name
	}
}

kdb::KeySet Plugin::getConfig ()
{
	return ksDup (elektraPluginGetConfig (plugin));
}

Plugin::Plugin (Plugin const & other)
: plugin (other.plugin), spec (other.spec), info (other.info), symbols (other.symbols), infos (other.infos), firstRef (other.firstRef)
{
	++plugin->refcounter;
}

Plugin & Plugin::operator= (Plugin const & other)
{
	if (this == &other) return *this;

	uninit ();

	plugin = other.plugin;
	spec = other.spec;
	info = other.info;
	symbols = other.symbols;
	infos = other.infos;
	firstRef = other.firstRef;

	++plugin->refcounter;

	return *this;
}

Plugin::~Plugin ()
{
	uninit ();
}

void Plugin::uninit ()
{
	/* ref counting will avoid closing */

	Key errorKey;
	ckdb::elektraPluginClose (plugin, errorKey.getKey ());
}

void Plugin::loadInfo ()
#if defined(__clang__)
	// We disable the undefined behavior sanitizer here, because otherwise the last line in this function produces the following error:
	// `runtime error: call to function (unknown) through pointer to incorrect function type`.
	// - See also: https://github.com/ElektraInitiative/libelektra/pull/1728
	// - TODO: Fix the undefined behavior
	__attribute__ ((no_sanitize ("undefined")))
#endif
{
	Key infoKey ("system:/elektra/modules", KEY_END);
	infoKey.addBaseName (spec.getName ());

	if (!plugin->kdbGet)
	{
		throw MissingSymbol ("kdbGet", plugin->name);
	}
	plugin->kdbGet (plugin, info.getKeySet (), *infoKey);
}

void Plugin::parse ()
{
	Key root ("system:/elektra/modules/", KEY_END);
	root.addBaseName (spec.getName ());

	Key k = info.lookup (root);
	if (!k)
	{
		throw PluginNoContract ();
	}

	root.addBaseName ("exports");

	ssize_t it = info.search (root) + 1;
	if (it > 0)
	{
		for (; it < info.size (); ++it)
		{
			k = info.at (it);
			if (!k.isBelow (root)) break;
			symbols[k.getName ().substr (root.getName ().length () + 1)] = (*k.getFunc ());
		}
	}

	root.setBaseName ("infos");

	it = info.search (root) + 1;
	if (it > 0)
	{
		for (; it < info.size (); ++it)
		{
			k = info.at (it);
			if (!k.isBelow (root)) break;
			infos[k.getName ().substr (root.getName ().length () + 1)] = k.getString ();
		}
	}
	else
	{
		throw PluginNoInfo ();
	}
}

void Plugin::check (vector<string> & warnings)
{
	if (infos.find ("version") == infos.end ())
		warnings.push_back ("no version found");
	else if (infos["version"] != PLUGINVERSION)
		throw VersionInfoMismatch ();

	if (infos.find ("licence") == infos.end ())
		warnings.push_back ("no licence information found");
	else if (infos["licence"] != "BSD")
		warnings.push_back ("the licence is not BSD, it might change the overall licence of your elektra installation");

	if (infos.find ("status") == infos.end ())
		warnings.push_back ("no status information found");
	else
	{
		// check if status is correct
		std::string statusString = infos["status"];
		std::istringstream ss (statusString);
		std::string status;
		while (ss >> status)
		{
			auto it = PluginDatabase::statusMap.find (status);
			if (it == PluginDatabase::statusMap.end ())
			{
				char * endptr;
				const char * str = status.c_str ();
				errno = 0;
				long val = strtol (str, &endptr, 10);
				if (((errno == ERANGE && (val > INT_MAX || val < INT_MIN)) || (errno != 0 && val == 0)) || endptr == str)
				{
					throw WrongStatus (status);
				}
			}
		}
	}

	if (infos.find ("description") == infos.end ()) warnings.push_back ("no description of the plugin found");

	if (infos.find ("provides") == infos.end ()) warnings.push_back ("no provides information found");
	if (infos.find ("placements") == infos.end ())
	{
		warnings.push_back ("no placements information found");
	}
	else
	{
		std::string placements = infos["placements"];
		if (placements.empty ())
		{
			warnings.push_back ("placements are empty");
		}

		if (placements == "backend" || placements == "hook")
		{
			// accepted
			// TODO (flo91): add checks in new mount tooling
		}
		else
		{
			std::vector<std::string> pp;
			pp.push_back ("prerollback");
			pp.push_back ("rollback");
			pp.push_back ("postrollback");
			pp.push_back ("getresolver");
			pp.push_back ("pregetcache");
			pp.push_back ("pregetstorage");
			pp.push_back ("getstorage");
			pp.push_back ("procgetstorage");
			pp.push_back ("postgetstorage");
			pp.push_back ("postgetcache");
			pp.push_back ("setresolver");
			pp.push_back ("postgetcleanup");
			pp.push_back ("presetstorage");
			pp.push_back ("setstorage");
			pp.push_back ("presetcleanup");
			pp.push_back ("precommit");
			pp.push_back ("commit");
			pp.push_back ("postcommit");
			istringstream is (placements);
			std::string placement;
			while (is >> placement)
			{
				if (std::find (pp.begin (), pp.end (), placement) == pp.end ())
				{
					warnings.push_back ("not supported placement " + placement + " found");
				}
			}
		}
	}
	if (infos.find ("needs") == infos.end ()) warnings.push_back ("no needs information found");

	if (infos.find ("author") == infos.end ())
	{
		warnings.push_back ("no author found");
	}
	else
	{
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
		if (symbols.find ("open") == symbols.end ())
			warnings.push_back ("no open symbol exported");
		else if (symbols["open"] != reinterpret_cast<func_t> (plugin->kdbOpen))
			throw SymbolMismatch ("open");
		ret = checkDups.insert (symbols["open"]);
		if (!ret.second) throw SymbolDuplicate ("open");
	}
	if (plugin->kdbClose)
	{
		if (symbols.find ("close") == symbols.end ())
			warnings.push_back ("no close symbol exported");
		else if (symbols["close"] != reinterpret_cast<func_t> (plugin->kdbClose))
			throw SymbolMismatch ("close");
		ret = checkDups.insert (symbols["close"]);
		if (!ret.second) throw SymbolDuplicate ("close");
	}
	if (plugin->kdbGet)
	{
		if (symbols.find ("get") == symbols.end ())
			warnings.push_back ("no get symbol exported");
		else if (symbols["get"] != reinterpret_cast<func_t> (plugin->kdbGet))
			throw SymbolMismatch ("get");
		ret = checkDups.insert (symbols["get"]);
		if (!ret.second) throw SymbolDuplicate ("get");
	}
	if (plugin->kdbSet)
	{
		if (symbols.find ("set") == symbols.end ())
			warnings.push_back ("no set symbol exported");
		else if (symbols["set"] != reinterpret_cast<func_t> (plugin->kdbSet))
			throw SymbolMismatch ("set");
		ret = checkDups.insert (symbols["set"]);
		if (!ret.second) throw SymbolDuplicate ("set");
	}
	if (plugin->kdbCommit)
	{
		if (symbols.find ("commit") == symbols.end ())
			warnings.push_back ("no commit symbol exported");
		else if (symbols["commit"] != reinterpret_cast<func_t> (plugin->kdbCommit))
			throw SymbolMismatch ("commit");
		ret = checkDups.insert (symbols["commit"]);
		if (!ret.second) throw SymbolDuplicate ("commit");
	}
	if (plugin->kdbError)
	{
		if (symbols.find ("error") == symbols.end ())
			warnings.push_back ("no error symbol exported");
		else if (symbols["error"] != reinterpret_cast<func_t> (plugin->kdbError))
			throw SymbolMismatch ("error");
		ret = checkDups.insert (symbols["error"]);
		if (!ret.second) throw SymbolDuplicate ("error");
	}
	if (symbols.find ("open") != symbols.end ())
	{
		if (!plugin->kdbOpen) throw SymbolMismatch ("open");
	}
	if (symbols.find ("close") != symbols.end ())
	{
		if (!plugin->kdbClose) throw SymbolMismatch ("close");
	}
	if (symbols.find ("get") != symbols.end ())
	{
		if (!plugin->kdbGet) throw SymbolMismatch ("get");
	}
	if (symbols.find ("set") != symbols.end ())
	{
		if (!plugin->kdbSet) throw SymbolMismatch ("set");
	}
	if (symbols.find ("commit") != symbols.end ())
	{
		if (!plugin->kdbCommit) throw SymbolMismatch ("commit");
	}
	if (symbols.find ("error") != symbols.end ())
	{
		if (!plugin->kdbError) throw SymbolMismatch ("error");
	}
}

ckdb::Plugin * Plugin::operator->()
{
	return plugin;
}

std::string Plugin::lookupInfo (std::string item, std::string section)
{
	Key k ("system:/elektra/modules", KEY_END);
	k.addBaseName (spec.getName ());
	k.addBaseName (section);
	k.addBaseName (item);
	Key ret = info.lookup (k);

	if (!ret) return ""; /* TODO Lets say missing info is ok for now */

	return ret.getString ();
}

bool Plugin::findInfo (std::string compare, std::string item, std::string section)
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

kdb::KeySet Plugin::getNeededConfig ()
{
	Key neededConfigKey ("system:/elektra/modules", KEY_END);
	neededConfigKey.addName (spec.getName ());
	neededConfigKey.addName ("config/needs");

	KeySet d (info.dup ());
	KeySet config = d.cut (neededConfigKey);

	KeySet ret;
	Key oldParent = neededConfigKey;
	Key newParent ("system:/", KEY_END);
	for (KeySet::iterator i = config.begin (); i != config.end (); ++i)
	{
		Key k (i->dup ());
		ret.append (kdb::tools::helper::rebaseKey (k, oldParent, newParent));
	}
	return ret;
}

int Plugin::open (kdb::Key & errorKey)
{
	if (!plugin->kdbOpen)
	{
		throw MissingSymbol ("kdbOpen", plugin->name);
	}

	return plugin->kdbOpen (plugin, errorKey.getKey ());
}

int Plugin::close (kdb::Key & errorKey)
{
	if (!plugin->kdbClose)
	{
		throw MissingSymbol ("kdbClose", plugin->name);
	}

	return plugin->kdbClose (plugin, errorKey.getKey ());
}

int Plugin::get (kdb::KeySet & ks, kdb::Key & parentKey)
#if defined(__clang__)
	// See `Plugin::loadInfo`
	__attribute__ ((no_sanitize ("undefined")))
#endif
{
	if (!plugin->kdbGet)
	{
		throw MissingSymbol ("kdbGet", plugin->name);
	}

	return plugin->kdbGet (plugin, ks.getKeySet (), parentKey.getKey ());
}

int Plugin::set (kdb::KeySet & ks, kdb::Key & parentKey)
#if defined(__clang__)
	// See `Plugin::loadInfo`
	__attribute__ ((no_sanitize ("undefined")))
#endif
{
	if (!plugin->kdbSet)
	{
		throw MissingSymbol ("kdbSet", plugin->name);
	}

	return plugin->kdbSet (plugin, ks.getKeySet (), parentKey.getKey ());
}

int Plugin::commit (kdb::KeySet & ks, kdb::Key & parentKey)
{
	if (!plugin->kdbCommit)
	{
		throw MissingSymbol ("kdbCommit", plugin->name);
	}

	return plugin->kdbCommit (plugin, ks.getKeySet (), parentKey.getKey ());
}

int Plugin::error (kdb::KeySet & ks, kdb::Key & parentKey)
{
	if (!plugin->kdbError)
	{
		throw MissingSymbol ("kdbError", plugin->name);
	}

	return plugin->kdbError (plugin, ks.getKeySet (), parentKey.getKey ());
}


std::string Plugin::name ()
{
	return spec.getName ();
}

std::string Plugin::getFullName ()
{
	return spec.getFullName ();
}

std::string Plugin::refname ()
{
	if (firstRef)
	{
		firstRef = false;
	}
	return spec.getRefName ();
}
} // namespace tools
} // namespace kdb

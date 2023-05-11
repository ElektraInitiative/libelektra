/**
 * @file
 *
 * @brief Implementation of backend
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <backend.hpp>
#include <backends.hpp>


#include <helper/keyhelper.hpp>
#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>

#include <kdbease.h> // for ckdb::elektraArrayIncName

#include <algorithm>

#include <cassert>
#include <kdb.hpp>


using namespace std;


namespace kdb
{


namespace tools
{

BackendInterface::~BackendInterface ()
{
}

MountBackendInterface::~MountBackendInterface ()
{
}

SerializeInterface::~SerializeInterface ()
{
}

/** Creates a new empty backend.
 *
 * */
Backend::Backend () : plugins ()
{
}


Backend::~Backend ()
{
}

Backend::Backend (Backend && other)
: getplugins (other.getplugins), setplugins (other.setplugins), errorplugins (other.errorplugins), commitplugins (other.commitplugins),
  mp (other.mp), configFile (other.configFile), modules (other.modules), config (other.config), plugins (std::move (other.plugins))
{
}

Backend & Backend::operator= (Backend && other)
{
	plugins = std::move (other.plugins);
	getplugins = other.getplugins;
	setplugins = other.setplugins;
	errorplugins = other.errorplugins;
	commitplugins = other.commitplugins;
	mp = other.mp;
	configFile = other.configFile;
	modules = other.modules;
	config = other.config;
	return *this;
}

/**
 * @brief Sets the mountpoint for the backend
 *
 * @throw MountpointInvalidException
 * @throw MountpointAlreadyInUseException
 *
 * @param mountpoint the key name will be used as mountpoint.
 *    It is allowed to pass a key with a cascading name.
 *
 * @param mountConf needs to include the keys below
 * system:/elektra/mountpoints
 */
void Backend::setMountpoint (Key mountpoint, KeySet mountConf)
{
	Backends::BackendInfoVector info = Backends::getBackendInfo (mountConf);
	std::string namesAsString;
	std::vector<std::string> alreadyUsedMountpoints;
	for (Backends::BackendInfoVector::const_iterator it = info.begin (); it != info.end (); ++it)
	{
		std::string const & name = it->mountpoint;
		if (name == "/")
		{
			alreadyUsedMountpoints.push_back ("spec:/");
			alreadyUsedMountpoints.push_back ("dir:/");
			alreadyUsedMountpoints.push_back ("user:/");
			alreadyUsedMountpoints.push_back ("system:/");
		}
		else if (name.at (0) == '/')
		{
			alreadyUsedMountpoints.push_back (Key ("dir:" + name, KEY_END).getName ());
			alreadyUsedMountpoints.push_back (Key ("user:" + name, KEY_END).getName ());
			alreadyUsedMountpoints.push_back (Key ("system:" + name, KEY_END).getName ());
		}

		// always add name itself, too
		alreadyUsedMountpoints.push_back (name);

		namesAsString += name;
		namesAsString += " ";
	}

	// STEP 0: check for null key
	if (!mountpoint)
	{
		throw MountpointAlreadyInUseException ("Null mountpoint not allowed");
	}

	std::string smp = mountpoint.getName ();

	// STEP 1: check for empty name
	if (smp.empty ())
	{
		throw MountpointAlreadyInUseException ("Empty mountpoint not allowed");
	}

	// STEP 2: check for wrong namespace (proc)
	if (mountpoint.getNamespace () == ElektraNamespace::PROC)
	{
		throw MountpointAlreadyInUseException ("proc:/ mountpoint not allowed");
	}

	// STEP 3: check for name match
	if (smp == "/")
	{
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), "/") != alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException (
				"Root mountpoint not possible, because the root mountpoint already exists.\n");
		}
		Key specmp ("spec:/", KEY_END);
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), specmp.getName ()) !=
		    alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException ("Root mountpoint not possible, because spec mountpoint already exists.\n");
		}
		Key dkmp ("dir:/", KEY_END);
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), dkmp.getName ()) !=
		    alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException ("Root mountpoint not possible, because dir mountpoint already exists.\n");
		}
		Key ukmp ("user:/", KEY_END);
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), ukmp.getName ()) !=
		    alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException ("Root mountpoint not possible, because user mountpoint already exists.\n");
		}
		Key skmp ("system:/", KEY_END);
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), skmp.getName ()) !=
		    alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException ("Root mountpoint not possible, because system mountpoint already exists.\n");
		}
	}
	else if (smp.at (0) == '/')
	{
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), smp) != alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException ("Cascading mountpoint " + smp +
							       " not possible, because cascading mountpoint " + smp + " already exists.\n");
		}
		Key dkmp ("dir:" + smp, KEY_END);
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), dkmp.getName ()) !=
		    alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException ("Cascading mountpoint " + smp +
							       " not possible, because dir mountpoint already exists.\n");
		}
		Key ukmp ("user:" + smp, KEY_END);
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), ukmp.getName ()) !=
		    alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException ("Cascading mountpoint " + smp +
							       " not possible, because user mountpoint already exists.\n");
		}
		Key skmp ("system:" + smp, KEY_END);
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), skmp.getName ()) !=
		    alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException ("Cascading mountpoint " + smp +
							       " not possible, because system mountpoint already exists.\n");
		}
	}
	else
	{
		Key kmp (smp, KEY_END);
		if (!kmp.isValid ()) throw MountpointInvalidException ();
		if (std::find (alreadyUsedMountpoints.begin (), alreadyUsedMountpoints.end (), kmp.getName ()) !=
		    alreadyUsedMountpoints.end ())
		{
			throw MountpointAlreadyInUseException (std::string ("Mountpoint ") + smp +
							       " is one of the already used names: " + namesAsString);
		}
	}

	// TODO STEP 4: check if mounted below system:/elektra
	Key elektraCheck (mountpoint.dup ());
	helper::removeNamespace (elektraCheck);
	if (elektraCheck.isBelowOrSame (Key ("/elektra", KEY_END)))
	{
		throw MountpointAlreadyInUseException (
			std::string ("Mountpoint ") + smp +
			" is below the reserved names /elektra because it would cause inconsistencies in this or future versions");
	}

	// everything worked, swap it
	std::swap (this->mp, smp);
}

/**
 * @brief Backend Config to add to
 *
 * @param ks the config to add, should be below system:/
 */
void Backend::setBackendConfig (KeySet const & ks)
{
	config.append (ks);
}


/**@pre: resolver needs to be loaded first
 * Will check the filename and use it as configFile for this backend.
 * @throw FileNotValidException if filename is not valid
 * @throw MissingSymbol if plugin does not implement 'checkfile' */
void Backend::useConfigFile (std::string file)
{
	typedef int (*checkFilePtr) (const char *);
	checkFilePtr checkFileFunction = nullptr;

	for (auto & elem : plugins)
	{
		try
		{
			checkFileFunction = reinterpret_cast<checkFilePtr> (elem->getSymbol ("checkfile"));
			break;
		}
		catch (MissingSymbol const & ms)
		{
		}
	}

	if (!checkFileFunction)
	{
		throw MissingSymbol ("No resolver with checkfile found", "");
	}


	int res = checkFileFunction (file.c_str ());

	if (res == -1) throw FileNotValidException ();

	configFile = file;
}


void Backend::tryPlugin (PluginSpec const & spec)
{
	PluginPtr plugin = modules.load (spec);

	errorplugins.tryPlugin (*plugin.get ());
	commitplugins.tryPlugin (*plugin.get ());
	getplugins.tryPlugin (*plugin.get ());
	setplugins.tryPlugin (*plugin.get ());


	for (auto & elem : plugins)
	{
		if (plugin->getFullName () == elem->getFullName ()) throw PluginAlreadyInserted (plugin->getFullName ());
	}


	plugins.push_back (std::move (plugin));
}


/**
 * Add a plugin that can be loaded, meets all
 * constraints.
 *
 * @note that this does not mean that the backend
 * validates after it is added. It only means that
 * the situation is not getting worse.
 *
 * @throw PluginCheckException or its subclasses if it was not possible
 * to load the plugin
 *
 * For validation @see validated().
 */
void Backend::addPlugin (PluginSpec const & plugin)
{
	KeySet fullPluginConfig = plugin.getConfig ();
	fullPluginConfig.append (plugin.getConfig ()); // add previous configs
	tryPlugin (plugin);
	commitplugins.addPlugin (*plugins.back ());
	errorplugins.addPlugin (*plugins.back ());
	getplugins.addPlugin (*plugins.back ());
	setplugins.addPlugin (*plugins.back ());

	KeySet toAdd = plugins.back ()->getNeededConfig ();
	config.append (toAdd);
}


/**
 * @return true if backend is validated
 * @return false if more plugins are needed to be valided
 */
bool Backend::validated () const
{
	bool ret = true;


	if (!commitplugins.validated ()) ret = false;
	if (!errorplugins.validated ()) ret = false;
	if (!getplugins.validated ()) ret = false;
	if (!setplugins.validated ()) ret = false;


	return ret;
}

void Backend::status (std::ostream & os) const
{
	if (validated ())
	{
		os << "No error, everything validated" << std::endl;
	}
	else
	{
		os << "Backend is not validated" << std::endl;
		if (!commitplugins.validated ())
		{
			os << "Commit Plugins are not validated" << std::endl;
		}

		if (!errorplugins.validated ())
		{
			os << "Error Plugins are not validated" << std::endl;
		}

		if (!getplugins.validated ())
		{
			os << "Get Plugins are not validated" << std::endl;
		}

		if (!setplugins.validated ())
		{
			os << "Set Plugins are not validated" << std::endl;
		}
	}
	errorplugins.status (os);
	commitplugins.status (os);
}

/**
 * @brief Prints the current status
 *
 * @param os stream to print to
 * @param b backend to get status from
 *
 * @return ref to stream
 */
std::ostream & operator<< (std::ostream & os, Backend const & b)
{
	b.status (os);
	return os;
}


/**
 * @pre name and mountpoint set
 * Add plugin serialization into keyset ret.
 *
 * Only can be done once!
 * (see firstRef in Plugin)
 * */
void Backend::serialize (kdb::KeySet & ret)
{
	assert (!mp.empty ());

	if (ret.lookup (Backends::getBasePath (mp)))
	{
		throw MountpointAlreadyInUseException ("mountpoint exists already");
	}

	Key backendRootKey = Key (Backends::getBasePath (mp), KEY_END);

	commitplugins.serialise (backendRootKey, ret);
	errorplugins.serialise (backendRootKey, ret);
	getplugins.serialise (backendRootKey, ret);
	setplugins.serialise (backendRootKey, ret);

	ret.append (Key (Backends::getBasePath (mp) + "/plugins/backend", KEY_END));
	ret.append (Key (Backends::getBasePath (mp) + "/plugins/backend/name", KEY_VALUE, "backend", KEY_END));
	ret.append (*Key (backendRootKey.getName () + "/definition/path", KEY_VALUE, configFile.c_str (), KEY_END));

	// If the path to the config file is an absolute path, automatically set the absolute key
	if (!configFile.empty() && configFile.at (0) == '/')
	{
		ret.append (*Key (backendRootKey.getName () + "/definition/path/absolute", KEY_VALUE, "true", KEY_END));
	}

	const string configBasePath = Backends::getBasePath (mp) + "/config";
	ret.append (Key (configBasePath, KEY_END));

	Key common = config.at (0);
	Key oldParent ("system:/", KEY_END);
	Key newParent (configBasePath, KEY_END);

	for (KeySet::iterator i = config.begin (); i != config.end (); ++i)
	{
		Key k (i->dup ());
		ret.append (kdb::tools::helper::rebaseKey (k, oldParent, newParent));
	}

	ret.append (backendRootKey);
}

void PluginAdder::addPlugin (PluginSpec const & spec)
{
	PluginPtr plugin = modules.load (spec);
	if (!plugin)
	{
		throw NoPlugin (spec.getName ());
	}
	std::shared_ptr<Plugin> sharedPlugin = std::move (plugin);

	std::istringstream ss (sharedPlugin->lookupInfo ("placements"));
	std::string placement;
	while (ss >> placement)
	{
		if (sharedPlugin->lookupInfo ("stacking") == "" && placement == "postgetstorage")
		{
			// reverse postgetstorage, except stacking is set
			plugins[placement].push_front (sharedPlugin);
		}
		else
		{
			plugins[placement].push_back (sharedPlugin);
		}
	}
}

namespace
{
void append (std::string placement, std::string & where, std::string checkPlacement)
{
	if (placement == checkPlacement)
	{
		if (where.empty ())
		{
			where = placement;
		}
		else
		{
			where += " ";
			where += placement;
		}
	}
}
} // namespace

struct Placements
{
	std::string get;
	std::string set;
	std::string error;

	void addPlacement (std::string placement)
	{
		append (placement, error, "prerollback");
		append (placement, error, "rollback");
		append (placement, error, "postrollback");

		append (placement, get, "getresolver");
		append (placement, get, "pregetstorage");
		append (placement, get, "getstorage");
		append (placement, get, "postgetstorage");

		append (placement, set, "setresolver");
		append (placement, set, "presetstorage");
		append (placement, set, "setstorage");
		append (placement, set, "precommit");
		append (placement, set, "commit");
		append (placement, set, "postcommit");
	}
};

namespace
{
Key g (Key placements, std::string name, std::string value)
{
	Key x (placements.dup ());
	x.addBaseName (name);
	x.setString (value);
	return x;
}

void serializeConf (kdb::KeySet & ret, Key config, KeySet const & pluginConfig)
{
	if (pluginConfig.size () != 0)
	{
		ret.append (config);
		for (auto const & key : pluginConfig)
		{
			Key k (key.dup ());
			helper::removeNamespace (k);
			ret.append (Key (config.getName () + k.getName (), KEY_VALUE, key.getString ().c_str (), KEY_END));
		}
	}
}
} // namespace

void GlobalPlugins::serialize (kdb::KeySet & ret)
{
	// transform to suitable data structure
	std::map<std::shared_ptr<Plugin>, Placements> pp;
	for (auto const & placements : plugins)
	{
		for (auto const & plugin : placements.second)
		{
			std::istringstream ss (plugin->lookupInfo ("status"));
			std::string status;
			bool isglobal = false;
			while (ss >> status)
			{
				if (status == "global") isglobal = true;
			}

			if (!isglobal)
			{
				throw NoGlobalPlugin (plugin->name ());
			}

			pp[plugin].addPlacement (placements.first);
		}
	}

	ret.append (Key ("system:/elektra/globalplugins", KEY_VALUE, "", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postcommit", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postcommit/user", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postcommit/user/placements", KEY_VALUE, "", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postcommit/user/placements/set", KEY_VALUE, "presetstorage precommit postcommit",
			 KEY_END));
	ret.append (
		Key ("system:/elektra/globalplugins/postcommit/user/placements/get", KEY_VALUE, "pregetstorage postgetstorage", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postcommit/user/placements/error", KEY_VALUE, "prerollback postrollback", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postcommit/user/plugins", KEY_VALUE, "", KEY_END));
	Key i ("system:/elektra/globalplugins/postcommit/user/plugins/#0", KEY_END);
	for (auto const & plugin : pp)
	{
		i.setString (plugin.first->name ());
		ret.append (i.dup ());
		Key placements (i.dup ());
		placements.addBaseName ("placements");
		ret.append (placements);

		ret.append (g (placements, "get", plugin.second.get));
		ret.append (g (placements, "set", plugin.second.set));
		ret.append (g (placements, "error", plugin.second.error));

		serializeConf (ret, Key (i.getName () + "/config", KEY_VALUE, "", KEY_END), plugin.first->getConfig ());
		ckdb::elektraArrayIncName (*i);
	}
	ret.append (Key ("system:/elektra/globalplugins/postgetcache", KEY_VALUE, "", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postgetcache", KEY_VALUE, "", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postgetcleanup", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/presetcleanup", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postrollback", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/precommit", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/pregetstorage", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/postgetstorage", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/presetstorage", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/prerollback", KEY_VALUE, "list", KEY_END));
	ret.append (Key ("system:/elektra/globalplugins/procgetstorage", KEY_VALUE, "list", KEY_END));
}

void ImportExportBackend::status (std::ostream & os) const
{
	if (plugins.empty ())
		os << "no plugin added" << std::endl;
	else if (plugins.find ("setstorage") == plugins.end ())
		os << "no storage plugin added" << std::endl;
	else
		os << "everything ok" << std::endl;
}

void ImportExportBackend::importFromFile (KeySet & ks, Key const & parentKey) const
{
	Key key = parentKey;
	std::vector<std::string> placements;
	placements.push_back ("getresolver");
	placements.push_back ("pregetstorage");
	placements.push_back ("getstorage");
	placements.push_back ("postgetstorage");
	for (auto const & placement : placements)
	{
		auto currentPlugins = plugins.find (placement);
		if (currentPlugins == plugins.end ()) continue;
		for (auto const & plugin : currentPlugins->second)
		{
			plugin->get (ks, key);
		}
	}
}

void ImportExportBackend::exportToFile (KeySet const & cks, Key const & parentKey) const
{
	KeySet ks = cks;
	Key key = parentKey;
	std::vector<std::string> placements;
	placements.push_back ("setresolver");
	placements.push_back ("presetstorage");
	placements.push_back ("setstorage");
	placements.push_back ("precommit");
	placements.push_back ("commit");
	placements.push_back ("postcommit");
	for (auto const & placement : placements)
	{
		auto currentPlugins = plugins.find (placement);
		if (currentPlugins == plugins.end ()) continue;
		for (auto const & plugin : currentPlugins->second)
		{
			plugin->set (ks, key);
		}
	}
}
} // namespace tools
} // namespace kdb

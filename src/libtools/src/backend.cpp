/**
 * \file
 *
 * @brief Implementation of backend
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */




#include <backend.hpp>
#include <backends.hpp>


#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>
#include <helper/keyhelper.hpp>

#include <algorithm>



#include <kdb.hpp>
#include <cassert>


using namespace std;


namespace kdb
{


namespace tools
{


/** Creates a new backend with a given name and mountpoint.
 * Parameters are needed for serialisation only, so you can
 * keep them empty if you do not want to serialise.
 *
 * */
Backend::Backend()
{
}


/**
 * @brief Sets the mountpoint for the backend
 *
 * @throw MountpointInvalidException
 * @throw MountpointAlreadyInUseException
 *
 * @param mountpoint the key name will be used as mountpoint.
 *    It is allowed to pass a key with a KEY_CASCADING_NAME
 *
 * @param mountConf needs to include the keys below
 * system/elektra/mountpoints
 */
void Backend::setMountpoint(Key mountpoint, KeySet mountConf)
{
	Backends::BackendInfoVector info = Backends::getBackendInfo(mountConf);
	std::string namesAsString;
	std::vector <std::string> alreadyUsedMountpoints;
	alreadyUsedMountpoints.push_back("system/elektra");
	for (Backends::BackendInfoVector::const_iterator it=info.begin();
			it!=info.end(); ++it)
	{
		std::string const & name = it->mountpoint;
		if (name == "/")
		{
			alreadyUsedMountpoints.push_back("spec");
			alreadyUsedMountpoints.push_back("dir");
			alreadyUsedMountpoints.push_back("user");
			alreadyUsedMountpoints.push_back("system");
		}
		else if (name.at(0) == '/')
		{
			alreadyUsedMountpoints.push_back(Key ("dir" + name, KEY_END).getName());
			alreadyUsedMountpoints.push_back(Key ("user" + name, KEY_END).getName());
			alreadyUsedMountpoints.push_back(Key ("system" + name, KEY_END).getName());
		}
		else
		{
			alreadyUsedMountpoints.push_back(name);
		}
		namesAsString += name;
		namesAsString += " ";
	}

	// STEP 0: check for null key
	if (!mountpoint)
	{
		throw MountpointAlreadyInUseException(
			"Null mountpoint not allowed");
	}

	std::string smp = mountpoint.getName();

	// STEP 1: check for empty name
	if (smp.empty())
	{
		throw MountpointAlreadyInUseException(
			"Empty mountpoint not allowed");
	}

	// STEP 2: check for wrong namespace (proc)
	if (mountpoint.getNamespace() == "proc")
	{
		throw MountpointAlreadyInUseException(
			"proc mountpoint not allowed");
	}

	// STEP 3: check for name match
	if (smp == "/")
	{
		Key specmp ("spec", KEY_END);
		if (std::find(alreadyUsedMountpoints.begin(), alreadyUsedMountpoints.end(), specmp.getName()) != alreadyUsedMountpoints.end())
		{
			throw MountpointAlreadyInUseException("Root mountpoint not possible, because spec mountpoint already exists");
		}
		Key dkmp ("dir", KEY_END);
		if (std::find(alreadyUsedMountpoints.begin(), alreadyUsedMountpoints.end(), dkmp.getName()) != alreadyUsedMountpoints.end())
		{
			throw MountpointAlreadyInUseException("Root mountpoint not possible, because dir mountpoint already exists");
		}
		Key ukmp ("user", KEY_END);
		if (std::find(alreadyUsedMountpoints.begin(), alreadyUsedMountpoints.end(), ukmp.getName()) != alreadyUsedMountpoints.end())
		{
			throw MountpointAlreadyInUseException("Root mountpoint not possible, because user mountpoint already exists");
		}
		Key skmp ("system", KEY_END);
		if (std::find(alreadyUsedMountpoints.begin(), alreadyUsedMountpoints.end(), skmp.getName()) != alreadyUsedMountpoints.end())
		{
			throw MountpointAlreadyInUseException("Root mountpoint not possible, because system mountpoint already exists");
		}
	} else if (smp.at(0) == '/')
	{
		Key dkmp ("dir" + smp, KEY_END);
		if (std::find(alreadyUsedMountpoints.begin(), alreadyUsedMountpoints.end(), dkmp.getName()) != alreadyUsedMountpoints.end())
		{
			throw MountpointAlreadyInUseException("Cascading mountpoint " +smp+ " not possible, because dir mountpoint already exists");
		}
		Key ukmp ("user" + smp, KEY_END);
		if (std::find(alreadyUsedMountpoints.begin(), alreadyUsedMountpoints.end(), ukmp.getName()) != alreadyUsedMountpoints.end())
		{
			throw MountpointAlreadyInUseException("Cascading mountpoint " +smp+ " not possible, because user mountpoint already exists");
		}
		Key skmp ("system" + smp, KEY_END);
		if (std::find(alreadyUsedMountpoints.begin(), alreadyUsedMountpoints.end(), skmp.getName()) != alreadyUsedMountpoints.end())
		{
			throw MountpointAlreadyInUseException("Cascading mountpoint " +smp+ " not possible, because system mountpoint already exists");
		}
	} else {
		Key kmp (smp, KEY_END);
		if (!kmp.isValid()) throw MountpointInvalidException();
		if (std::find(alreadyUsedMountpoints.begin(), alreadyUsedMountpoints.end(), kmp.getName()) != alreadyUsedMountpoints.end())
		{
			throw MountpointAlreadyInUseException(
				std::string("Mountpoint ") +
				smp +
				" is one of the already used names: " +
				namesAsString
				);
		}
	}

	// everything worked, swap it
	std::swap(this->mp, smp);
}

/**
 * @brief Backend Config to add to
 *
 * @param ks the config to add, should be below system/
 */
void Backend::setBackendConfig (KeySet const & ks)
{
	config.append(ks);
}

Backend::~Backend()
{
	for (size_t i = 0; i < plugins.size(); ++i)
	{
		delete plugins[i];
	}
}


/**@pre: resolver needs to be loaded first
 * Will check the filename and use it as configFile for this backend.
 * @throw FileNotValidException if filename is not valid */
void Backend::useConfigFile(std::string file)
{
	typedef int (*checkFilePtr) (const char*);
	checkFilePtr checkFileFunction = 0;

	for (size_t i = 0; i < plugins.size(); ++i)
	{
		try {
			checkFileFunction =
				reinterpret_cast<checkFilePtr>(plugins[i]->getSymbol("checkfile"));
			break;
		}
		catch(MissingSymbol ms)
		{}
	}

	if (!checkFileFunction)
	{
		throw MissingSymbol("No resolver with checkfile found");
	}


	int res = checkFileFunction(file.c_str());

	if (res == -1) throw FileNotValidException();

	configFile = file;
}


void Backend::tryPlugin (std::string pluginName, KeySet pluginConfig)
{
	int nr;
	char *cPluginName = 0;
	char *cReferenceName = 0;
	Key errorKey;
	string realPluginName;


	Key k(std::string("system/elektra/key/#0") + pluginName, KEY_END);


	if (ckdb::elektraProcessPlugin (*k, &nr, &cPluginName, &cReferenceName, *errorKey) == -1)
	{
		ckdb::elektraFree(cPluginName);
		ckdb::elektraFree(cReferenceName);
		throw BadPluginName();
	}


	if (cPluginName)
	{
		realPluginName = cPluginName;
		ckdb::elektraFree(cPluginName);
	}


	if (realPluginName.find('#') != string::npos) throw BadPluginName();



	PluginPtr plugin = modules.load(realPluginName, pluginConfig);


	// because PluginPtr might be auto_ptr we cannot make that more
	// pretty:
	errorplugins.tryPlugin (*plugin.get());
	getplugins.tryPlugin   (*plugin.get());
	setplugins.tryPlugin   (*plugin.get());


	for (size_t i=0; i<plugins.size(); ++i)
	{
		if (plugin->name() == plugins[i]->name())
			throw PluginAlreadyInserted();
	}


	plugins.push_back(plugin.release());
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
void Backend::addPlugin (std::string pluginName, KeySet pluginConf)
{
	KeySet fullPluginConfig = pluginConf;
	fullPluginConfig.append(config); // add previous configs
	tryPlugin (pluginName, fullPluginConfig);
	errorplugins.addPlugin (*plugins.back());
	getplugins.addPlugin (*plugins.back());
	setplugins.addPlugin (*plugins.back());

	KeySet toAdd = plugins.back()->getNeededConfig();
	config.append(toAdd);
}


/**
 * @return true if backend is validated
 * @return false if more plugins are needed to be valided
 */
bool Backend::validated () const
{
	bool ret = true;


	if (!errorplugins.validated()) ret = false;
	if (!getplugins.validated()) ret = false;
	if (!setplugins.validated()) ret = false;


	return ret;
}

void Backend::status (std::ostream & os) const
{
	if (validated())
	{
		os << "No error, everything validated" << std::endl;
	}
	else
	{
		os << "Backend is not validated" << std::endl;
		if (!errorplugins.validated())
		{
			os << "Error Plugins are not validated" << std::endl;
		}

		if (!getplugins.validated())
		{
			os << "Get Plugins are not validated" << std::endl;
		}

		if (!setplugins.validated())
		{
			os << "Set Plugins are not validated" << std::endl;
		}

	}
	errorplugins.status(os);
}

/**
 * @brief Prints the current status
 *
 * @param os stream to print to
 * @param b backend to get status from
 *
 * @return ref to stream
 */
std::ostream & operator<<(std::ostream & os, Backend const & b)
{
	b.status(os);
	return os;
}


/**
 * @pre name and mountpoint set
 * Add plugin serialization into keyset ret.
 *
 * Only can be done once!
 * (see firstRef in Plugin)
 * */
void Backend::serialize (kdb::KeySet &ret)
{
	assert(!mp.empty());
	Key backendRootKey (Backends::mountpointsPath, KEY_END);
	backendRootKey.addBaseName (mp);
	backendRootKey.setString("This is a configuration for a backend, see subkeys for more information");
	ret.append(backendRootKey);


	if (mp == "/")
	{
		ret.append ( *Key(	backendRootKey.getName() + "/mountpoint",
				KEY_VALUE, "/",
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"This is the root mountpoint.\n",
				KEY_END));
	}
	else if (mp.at(0) == '/')
	{
		Key k("system" + mp, KEY_END);
		Key restrictedPath ("system/elektra", KEY_END);
		if (!k) throw MountpointInvalidException();
		if (restrictedPath.isBelow(k)) throw MountpointInvalidException();
		ret.append ( *Key(	backendRootKey.getName() + "/mountpoint",
				KEY_VALUE, mp.c_str(),
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"This is a cascading mountpoint.\n"
				"That means it is both mounted to dir, user and system.",
				KEY_END));
	} else {
		Key k(mp, KEY_END);
		Key restrictedPath ("system/elektra", KEY_END);
		if (!k) throw MountpointInvalidException();
		if (restrictedPath.isBelow(k)) throw MountpointInvalidException();
		ret.append ( *Key(	backendRootKey.getName() + "/mountpoint",
				KEY_VALUE, mp.c_str(),
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"This is a normal mountpoint.\n",
				KEY_END));
	}

	const string configBasePath = Backends::getBasePath (mp) + "/config";
	ret.append(Key(configBasePath, KEY_END));

	config.rewind();
	Key common = config.next();
	Key oldParent("system", KEY_END);
	Key newParent(configBasePath, KEY_END);

	for (KeySet::iterator i = config.begin(); i != config.end(); ++i)
	{
		Key k(i->dup());
		ret.append(kdb::tools::helper::rebaseKey(k, oldParent, newParent));
	}


	errorplugins.serialise(backendRootKey, ret);
	getplugins.serialise(backendRootKey, ret);
	setplugins.serialise(backendRootKey, ret);

	ret.append ( *Key(backendRootKey.getName()+"/config/path",
			KEY_VALUE, configFile.c_str(),
			KEY_COMMENT, "The path for this backend. Note that plugins can override that with more specific configuration.",
			KEY_END));
}


}


}


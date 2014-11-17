/**
 * \file
 *
 * \brief Implementation of backend
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */




#include <backend.hpp>
#include <backends.hpp>


#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>

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
 *    It is allowed to pass a key with a KDB_O_CASCADING_NAME
 *
 * @param mountConf needs to include the keys below
 * system/elektra/mountpoints
 */
void Backend::setMountpoint(Key mountpoint, KeySet mountConf)
{
	std::vector <std::string> names;
	std::string namesInString;
	Backends::BackendInfoVector info = Backends::getBackendInfo(mountConf);
	names.push_back("default");
	for (Backends::BackendInfoVector::const_iterator it=info.begin();
			it!=info.end(); ++it)
	{
		names.push_back(it->mountpoint);
		namesInString += it->mountpoint;
	}


	if (std::find(names.begin(), names.end(), mountpoint.getName()) != names.end())
	{
		throw MountpointAlreadyInUseException(
			std::string("Mountpoint ") + 
			mountpoint.getName() +
			" is one of the already used names: " +
			namesInString
			);
	}

	std::vector <std::string> mountpoints;
	mountpoints.push_back("system/elektra");
	mountConf.rewind();
	Key cur;
	while ((cur = mountConf.next()))
	{
		if (cur.getBaseName() == "mountpoint")
		{
			if (cur.getString().at(0) == '/')
			{
				mountpoints.push_back(Key ("user" + cur.getString(), KEY_END).getName());
				mountpoints.push_back(Key ("system" + cur.getString(), KEY_END).getName());
			}
		};
	}

	if (mp.at(0) == '/')
	{
		Key skmp ("system" + mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), skmp.getName()) != mountpoints.end())
		{
			throw MountpointAlreadyInUseException("Cascading mountpoint not possible, because system mountpoint already exists");
		}
		Key ukmp ("user" + mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), ukmp.getName()) != mountpoints.end())
		{
			throw MountpointAlreadyInUseException("Cascading mountpoint not possible, because user mountpoint already exists");
		}
	} else {
		Key kmp (mp, KEY_END);
		if (!kmp.isValid()) throw MountpointInvalidException();
		if (std::find(mountpoints.begin(), mountpoints.end(), kmp.getName()) != mountpoints.end())
		{
			throw MountpointAlreadyInUseException(
				std::string("Mountpoint ") + 
				mountpoint.getName() +
				" is one of the already used cascading names: " +
				namesInString
				);
		}
	}

	mp = mountpoint.getName();
	name = mountpoint.getName();
	std::replace(name.begin(), name.end(), '/', '_');
}


Backend::~Backend()
{
	for (size_t i = 0; i < plugins.size(); ++i)
	{
		delete plugins[i];
	}
}


/**@pre: resolver needs to be loaded first
 * Will check the filename.
 * @throw FileNotValidException if filename is not valid */
void Backend::checkFile (std::string file) const
{
	typedef int (*checkFilePtr) (const char*);
	checkFilePtr checkFileFunction = (checkFilePtr) plugins.back()->getSymbol("checkfile");
	assert(checkFileFunction);


	int res = checkFileFunction(file.c_str());

	if (res == -1) throw FileNotValidException();
}


void Backend::tryPlugin (std::string pluginName)
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




	KeySet testConfig(1,
		*Key(	"system/test",
			KEY_VALUE, "test",
			KEY_COMMENT, "Test config for loading a plugin.",
			KEY_END),
		KS_END);


	PluginPtr plugin = modules.load(realPluginName, testConfig);


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
void Backend::addPlugin (std::string pluginName)
{
	tryPlugin (pluginName);
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
 * Write plugin into keyset ret below rootKey. */
void Backend::serialise (kdb::Key &rootKey, kdb::KeySet &ret)
{
	assert(!name.empty());
	assert(!mp.empty());
	Key backendRootKey (rootKey);
	backendRootKey.addBaseName (name);
	backendRootKey.setString("serialised Backend");
	ret.append(backendRootKey);


	if (mp == "/")
	{
		ret.append ( *Key(	rootKey.getName() + "/mountpoint",
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
		ret.append ( *Key(	rootKey.getName() + "/mountpoint",
				KEY_VALUE, mp.c_str(),
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"This is a cascading mountpoint.\n"
				"That means it is both mounted to user and system.",
				KEY_END));
	} else {
		Key k(mp, KEY_END);
		Key restrictedPath ("system/elektra", KEY_END);
		if (!k) throw MountpointInvalidException();
		if (restrictedPath.isBelow(k)) throw MountpointInvalidException();
		ret.append ( *Key(	rootKey.getName() + "/mountpoint",
				KEY_VALUE, mp.c_str(),
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"This is a normal mountpoint.\n",
				KEY_END));
	}


	config.rewind();
	Key common = config.next();
	if (common)
	{
		string commonName = common.getName();

		while (Key k = config.next())
		{
			string newName = k.getName().substr (commonName.length());
			Key x(k.dup());
			x.setName(backendRootKey.getName());
			x.addBaseName("config");
			x.addBaseName(newName);
			ret.append (x);
		}
	}


	errorplugins.serialise(backendRootKey, ret);
	getplugins.serialise(backendRootKey, ret);
	setplugins.serialise(backendRootKey, ret);
}


}


}


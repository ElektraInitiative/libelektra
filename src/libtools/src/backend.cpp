/**
 * \file
 *
 * \brief Implementation of backend
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <backend.hpp>

#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>

#include <kdb.hpp>
#include <cassert>

#include <iostream>
#include <memory>

using namespace std;

namespace kdb
{

/** Creates a new backend with a given name and mountpoint.
 * Parameters are needed for serialisation only, so you can
 * keep them empty if you do not want to serialise. */
Backend::Backend(string name_ = "", string mp_ = "") :
	name(name_), mp(mp_)
{
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
void Backend::checkFile (std::string file)
{
	typedef int (*checkFilePtr) (const char*);
	checkFilePtr checkFileFunction = (checkFilePtr) plugins.back()->getSymbol("checkfile");
	assert(checkFileFunction);

	int res = checkFileFunction(file.c_str());

	if (mp.substr(0,6) == "system")
	{
		if (res == -1) throw FileNotValidException();
		return;
	}

	if (res <= 0) throw FileNotValidException();
}

/** Try if a plugin can be loaded, meets all safety
 * constraints and could be added.
 *
 * @note that this does not mean that the backend
 * validates after it is added. It only means that
 * the situation is not getting worse.
 *
 * @throw PluginCheckException or its subclasses
 *
 * For validation @see validated().
 */
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
		cout << "# seems like there is a pluginName: " << realPluginName << endl;
	}

	if (realPluginName.find('#') != string::npos) throw BadPluginName();


	KeySet testConfig(1,
		*Key(	"system/test",
			KEY_VALUE, "test",
			KEY_COMMENT, "Test config for loading a plugin.",
			KEY_END),
		KS_END);

	kdb::PluginPtr plugin = modules.load(realPluginName, testConfig);
	vector<string> warnings;
	plugin->check(warnings);

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

	if (warnings.size() > 0)
	{
		cerr << "There are " << warnings.size() << " Warnings for this plugin" << endl;

		for (size_t i = 0; i < warnings.size(); ++i)
		{
			cerr << "Warning #" << i << ": " << warnings[i] << endl;
		}
	}

	plugins.push_back(plugin.release());
}

/** Add the plugin which were tried the last time.
 * @pre tryPlugin was successful first (did not throw) */
void Backend::addPlugin ()
{
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
bool Backend::validated ()
{
	bool ret = true;

	if (!errorplugins.validated()) ret = false;
	if (!getplugins.validated()) ret = false;
	if (!setplugins.validated()) ret = false;

	return ret;
}

/**
 * @pre name and mountpoint set
 * Write plugin into keyset ret below rootKey. */
void Backend::serialize (kdb::Key &rootKey, kdb::KeySet &ret)
{
	assert(!name.empty());
	assert(!mp.empty());
	Key backendRootKey (rootKey);
	backendRootKey.addBaseName (name);
	backendRootKey.setString("serialized Backend");
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

		// TODO commonName might be too long if config/needs key is missing

		while (Key k = config.next())
		{
			string newName = k.getName().substr (commonName.length());
			Key x (k);
			x.setName(backendRootKey.getName());
			x.addBaseName("config");
			x.addBaseName(newName);
			ret.append (x);
		}
	}

	errorplugins.serialize(backendRootKey, ret);
	getplugins.serialize(backendRootKey, ret);
	setplugins.serialize(backendRootKey, ret);
}

}

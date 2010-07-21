#include <backend.hpp>

#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>

#include <kdb>

#include <iostream>
#include <memory>

using namespace std;
using namespace kdb;

Backend::Backend(string name, string mp) :
	name(name), mp(mp)
{
	elektraModulesInit(modules.getKeySet(), 0);
}

Backend::~Backend()
{
	elektraModulesClose(modules.getKeySet(), 0);
	for (size_t i = 0; i < plugins.size(); ++i)
	{
		delete plugins[i];
	}
}

void Backend::checkFile (std::string file)
{
	typedef int (*checkFilePtr) (const char*);
	checkFilePtr checkFile = (checkFilePtr) plugins.back()->getSymbol("checkfile");

	int res = checkFile(file.c_str());

	cout << "Result is: " << res << endl;

	if (mp.substr(0,6) == "system")
	{
		cout << "Absolut filename is ok: " << file;
		if (res == -1) throw FileNotValidException();
		return;
	}

	if (res <= 0)
		throw FileNotValidException();
}

/** Try if a plugin can be loaded, meets all safety
 * constraints and could be added.
 *
 * @note that this does not mean that the backend
 * validates after it is added. It only means that
 * the situation is not getting worse.
 *
 * For validation see validated().
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

	auto_ptr<Plugin>plugin (new Plugin (realPluginName, modules, testConfig));
	plugin->loadInfo();
	plugin->parse();
	vector<string> warnings;
	plugin->check(warnings);

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

/** Add the plugin which were tried the last time */
void Backend::addPlugin ()
{
	errorplugins.addPlugin (*plugins.back());
	getplugins.addPlugin (*plugins.back());
	setplugins.addPlugin (*plugins.back());
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

void Backend::serialize (kdb::Key &rootKey, kdb::KeySet &ret)
{
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

	errorplugins.serialize(backendRootKey, ret);
	getplugins.serialize(backendRootKey, ret);
	setplugins.serialize(backendRootKey, ret);
}

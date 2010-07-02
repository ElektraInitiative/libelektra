#include <backend.hpp>

#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>

#include <kdb>

#include <iostream>
#include <memory>

using namespace std;
using namespace kdb;

Backend::Backend(string name) :
	name(name)
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

	Key k(std::string("system/elektra/key/#0") + pluginName);

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

	errorplugins.serialize(backendRootKey, ret);
	getplugins.serialize(backendRootKey, ret);
	setplugins.serialize(backendRootKey, ret);
}

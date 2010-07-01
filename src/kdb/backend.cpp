#include <backend.hpp>

#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>

#include <kdb>

#include <iostream>

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

void Backend::addPlugin (std::string pluginName)
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
	plugins.push_back(new Plugin (realPluginName, modules, testConfig));

	errorplugins.addPlugin (*plugins.back());
	getplugins.addPlugin (*plugins.back());
	setplugins.addPlugin (*plugins.back());
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

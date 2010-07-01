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
	Plugin plugin (realPluginName, modules, testConfig);

	getplugins.addPlugin (plugin);
	setplugins.addPlugin (plugin);
	errorplugins.addPlugin (plugin);
}

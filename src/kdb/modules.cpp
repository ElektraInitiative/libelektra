#include <modules.hpp>
#include <keyset.hpp>

#include <kdbmodule.h>
#include <kdbplugin.h>

#include <memory>

using namespace kdb;
using namespace std;

Modules::Modules()
{
	ckdb::elektraModulesInit(modules.getKeySet(), 0);
}

Modules::~Modules()
{
	ckdb::elektraModulesClose(modules.getKeySet(), 0);
}

std::auto_ptr<Plugin> Modules::load(std::string const& pluginName)
{
	KeySet config(1,
		*Key(	"system/test",
			KEY_VALUE, "test",
			KEY_COMMENT, "Test config for loading a plugin.",
			KEY_END),
		KS_END);

	return load(pluginName, config);
}

std::auto_ptr<Plugin> Modules::load(std::string const& pluginName, KeySet const & config)
{
	std::auto_ptr<Plugin> plugin (new Plugin (pluginName, modules, config));
	plugin->loadInfo();
	plugin->parse();
	vector<string> warnings;
	plugin->check(warnings);

	return plugin;
}

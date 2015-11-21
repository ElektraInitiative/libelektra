/**
 * @file
 *
 * @brief Implementation of module loading
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <modules.hpp>
#include <keyset.hpp>

#include <kdbmodule.h>
#include <kdbplugin.h>

using namespace std;

namespace kdb
{

namespace tools
{

Modules::Modules()
{
	ckdb::elektraModulesInit(modules.getKeySet(), 0);
}

Modules::~Modules()
{
	ckdb::elektraModulesClose(modules.getKeySet(), 0);
}

PluginPtr Modules::load(std::string const& pluginName)
{
	KeySet config(1,
		*Key(	"system/module",
			KEY_VALUE, "this plugin was loaded without a config",
			KEY_END),
		KS_END);

	return load(pluginName, config);
}

PluginPtr Modules::load(std::string const& pluginName, KeySet const & config)
{
	PluginPtr plugin (new Plugin (pluginName, modules, config));
	plugin->loadInfo();
	plugin->parse();

	return plugin;
}

}

}

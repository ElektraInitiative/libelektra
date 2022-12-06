/**
 * @file
 *
 * @brief Implementation of module loading
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <keyset.hpp>
#include <modules.hpp>

#include <elektra/kdbmodule.h>
#include <elektra/kdbplugin.h>

using namespace std;

namespace kdb
{

namespace tools
{

Modules::Modules ()
{
	ckdb::elektraModulesInit (modules.getKeySet (), nullptr);
}

Modules::~Modules ()
{
	ckdb::elektraModulesClose (modules.getKeySet (), nullptr);
}

PluginPtr Modules::load (std::string const & pluginName)
{
	KeySet config (1, *Key ("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END);

	return load (pluginName, config);
}

PluginPtr Modules::load (std::string const & pluginName, KeySet const & config)
{
	return load (PluginSpec (pluginName, config));
}

PluginPtr Modules::load (PluginSpec const & spec)
{
	PluginPtr plugin (new Plugin (spec, modules));
	plugin->loadInfo ();
	plugin->parse ();

	return plugin;
}
} // namespace tools
} // namespace kdb

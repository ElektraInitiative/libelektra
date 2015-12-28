/**
 * @file
 *
 * @brief Interface to specify which plugin is meant
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_PLUGIN_SPEC_HPP
#define TOOLS_PLUGIN_SPEC_HPP

#include <kdb.hpp>

namespace kdb
{

namespace tools
{

struct PluginSpec
{
	PluginSpec(
		std::string pluginName,
		KeySet pluginConfig = KeySet()) :
		name(pluginName),
		config(pluginConfig)
	{}

	std::string name;
	KeySet config;
};

bool operator == (PluginSpec const & self, PluginSpec const & other)
{
	return self.name == other.name &&
		std::equal(self.config.begin(), self.config.end(),
				other.config.begin());
}

typedef std::vector <PluginSpec> PluginSpecVector;

}

}

#endif

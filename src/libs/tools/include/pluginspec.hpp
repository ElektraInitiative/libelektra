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

#include <vector>
#include <string>

#include <kdb.hpp>

namespace kdb
{

namespace tools
{

struct PluginSpec
{
	explicit PluginSpec(
		std::string pluginName,
		KeySet pluginConfig = KeySet()) :
		name(pluginName),
		config(pluginConfig)
	{}

	std::string name;
	KeySet config;
};

inline bool operator == (PluginSpec const & self, PluginSpec const & other)
{
	return self.name == other.name &&
		std::equal(self.config.begin(), self.config.end(),
				other.config.begin());
}

typedef std::vector <PluginSpec> PluginSpecVector;

}

}

namespace std
{
	// produces hash collisions if only config differs
	template <> struct hash<kdb::tools::PluginSpec>
	{
		size_t operator()(kdb::tools::PluginSpec const & s) const
		{
			return std::hash<std::string>()(s.name);
		}
	};
} // end of namespace std

#endif

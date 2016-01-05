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
		refname(pluginName),
		config(pluginConfig)
	{
		auto it = pluginName.find ('#');
		if (it != std::string::npos)
		{
			refname = pluginName.substr(it+1);
			name = pluginName.substr(0,it);
		}
	}

	explicit PluginSpec(
		std::string pluginName,
		std::string refName,
		KeySet pluginConfig = KeySet()) :
		name(pluginName),
		refname(refName),
		config(pluginConfig)
	{}

	std::string name;
	std::string refname;
	KeySet config;
};

inline bool operator == (PluginSpec const & self, PluginSpec const & other)
{
	return self.name == other.name && self.refname == other.refname;
}

inline bool operator != (PluginSpec const & self, PluginSpec const & other)
{
	return !(self == other);
}

typedef std::vector <PluginSpec> PluginSpecVector;

inline std::ostream & operator << (std::ostream & os, PluginSpec const & spec)
{
	os << "name: " << spec.name << " refname: " << spec.refname << " configsize: " << spec.config.size();
	return os;
}

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

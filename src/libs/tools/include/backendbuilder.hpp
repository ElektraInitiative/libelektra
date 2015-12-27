/**
 * @file
 *
 * @brief Implements a way to build backends
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_BACKEND_BUILDER_HPP
#define TOOLS_BACKEND_BUILDER_HPP

#include <kdb.hpp>

#include <memory>

class Backend;

namespace kdb
{

namespace tools
{

/**
 * @brief Highlevel interface to build a backend.
 *
 * Automatically reorders plugins and has different modes which Backend
 * should be built.
 */
class BackendBuilder
{
public:
	struct PluginSpec
	{
		PluginSpec(
			std::string pluginName,
			KeySet pluginConf = KeySet()) :
			name(pluginName),
			conf(pluginConf)
		{}

		bool operator == (PluginSpec const & other)
		{
			return name == other.name &&
				std::equal(conf.begin(), conf.end(), other.conf.begin());
		}

		std::string name;
		KeySet conf;
	};

private:
	std::vector <PluginSpec> toAdd;

public:
	BackendBuilder();
	~BackendBuilder();

	void parseArguments (std::string const & cmdline);
	void addPlugin (PluginSpec plugin);
	void remPlugin (PluginSpec plugin);
	void status (std::ostream & os) const;
	bool validated () const;
	Backend create() const;
};

}

}

#endif

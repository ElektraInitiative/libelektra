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
			KeySet pluginConfig = KeySet()) :
			name(pluginName),
			config(pluginConfig)
		{}

		bool operator == (PluginSpec const & other)
		{
			return name == other.name &&
				std::equal(config.begin(), config.end(),
						other.config.begin());
		}

		std::string name;
		KeySet config;
	};

private:
	/// Defines order in which plugins should be added
	std::vector <PluginSpec> toAdd;

public:
	BackendBuilder();
	~BackendBuilder();

	void sort();
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

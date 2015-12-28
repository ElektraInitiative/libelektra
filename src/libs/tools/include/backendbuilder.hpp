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

struct PluginSpec
{
	// TODO allow to mock

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

/**
 * @brief Highlevel interface to build a backend.
 *
 * Automatically reorders plugins and has different modes which Backend
 * should be built.
 */
class BackendBuilder
{
private:
	/// Defines order in which plugins should be added
	PluginSpecVector toAdd;

	void sort();

public:
	BackendBuilder();
	~BackendBuilder();

	static KeySet parsePluginArguments (std::string const & pluginArguments);
	static PluginSpecVector parseArguments (std::string const & cmdline);

	void addPlugins (PluginSpecVector plugin);
	void addPlugin (PluginSpec plugin);
	void remPlugin (PluginSpec plugin);
	void status (std::ostream & os) const;
	bool validated () const;
	Backend create() const;
};

}

}

#endif

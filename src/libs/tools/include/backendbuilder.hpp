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

#include <pluginspec.hpp>

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

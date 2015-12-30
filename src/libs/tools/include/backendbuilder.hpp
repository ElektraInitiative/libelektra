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

#include <memory>

namespace kdb
{

namespace tools
{

class Backend;
class PluginDatabase;

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
	typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;
	PluginDatabasePtr pluginDatabase;

	void sort();

public:
	BackendBuilder();
	explicit BackendBuilder(PluginDatabasePtr const & pluginDatabase);

	PluginSpec const & operator[] (size_t pos) const
	{
		return toAdd[pos];
	}

	size_t size() const
	{
		return toAdd.size();
	}

	PluginDatabasePtr getPluginDatabase() const
	{
		return pluginDatabase;
	}

	~BackendBuilder();

	static KeySet parsePluginArguments (std::string const & pluginArguments);
	static PluginSpecVector parseArguments (std::string const & cmdline);

	void addPlugins (PluginSpecVector plugin);
	void addPlugin (PluginSpec plugin);
	void remPlugin (PluginSpec plugin);
	void status (std::ostream & os) const;
	bool validated () const;
	void resolveNeeds();
	Backend create() const;
	void create(BackendInterface & b) const;
};

}

}

#endif

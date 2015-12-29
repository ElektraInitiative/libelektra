/**
 * @file
 *
 * @brief Interface to all plugins
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_PLUGIN_DATABASE_HPP
#define TOOLS_PLUGIN_DATABASE_HPP

#include <memory>

#include <kdb.hpp>

#include <pluginspec.hpp>

namespace kdb
{

namespace tools
{

/**
 * @brief Loads all plugins and allows us to query them.
 */
class PluginDatabase
{
public:
	virtual std::string lookupInfo (PluginSpec const & whichplugin, std::string const & which) const = 0;
};

typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;

class ModulesPluginDatabase : public PluginDatabase
{
	class Impl;
	std::unique_ptr<Impl> impl;
public:
	ModulesPluginDatabase ();
	~ModulesPluginDatabase ();
	std::string lookupInfo (PluginSpec const & spec, std::string const & which) const;
};

}

}

#endif

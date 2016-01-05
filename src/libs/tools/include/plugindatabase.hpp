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
#include <unordered_map>

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
	/**
	 * @brief list all plugins
	 *
	 * If Elektra is compiled with plugins, it will search for shared libraries.
	 * In any case, if no shared libraries were found
	 * it will fallback to an internal list (plugins that were compiled together with Elektra).
	 *
	 * @return a list of all available plugins
	 */
	virtual std::vector<std::string> listAllPlugins() const = 0;

	// TODO: add additional functions:
	// virtual void registerPlugin(PluginSpec) = 0;
	// virtual std::vector<PluginSpec> listAllPlugins() const = 0;

	/**
	 * @brief lookup contract clauses or dynamic information
	 *
	 * Dynamic information is one of:
	 *
	 * - "exists" ... returns "real" or "no"
	 *
	 * @param whichplugin about which plugin?
	 * @param which about which clause in the contract?
	 *
	 * @return the clause of the contract
	 */
	virtual std::string lookupInfo (PluginSpec const & whichplugin, std::string const & which) const = 0;

	/**
	 * @brief lookup which plugin handles meta data
	 *
	 * @param which the meta data of interest
	 *
	 * @return the best suited plugin specification which provides it
	 */
	virtual PluginSpec lookupMetadata (std::string const & which) const = 0;

	/**
	 * @brief lookup which plugin is a provider for that plugin
	 *
	 * @note will return a PluginSpec with getName() == provides if the string provides
	 *       actually is a plugin name.
	 *
	 * @param provides is the provider to find
	 *
	 * @throw NoPlugin if no plugin that provides the functionality could be found
	 *
	 * @return the plugin itself or the best suited plugin specification which provides it
	 */
	virtual PluginSpec lookupProvides (std::string const & provides) const = 0;
};

typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;

class ModulesPluginDatabase : public PluginDatabase
{
	class Impl;
	std::unique_ptr<Impl> impl;
public:
	ModulesPluginDatabase ();
	~ModulesPluginDatabase ();

	std::vector<std::string> listAllPlugins() const;
	std::string lookupInfo (PluginSpec const & spec, std::string const & which) const;
	PluginSpec lookupMetadata (std::string const & which) const;
	PluginSpec lookupProvides (std::string const & provides) const;
};

class MockPluginDatabase : public ModulesPluginDatabase
{
public:
	/// only data from here will be returned
	mutable std::unordered_map <PluginSpec, std::unordered_map<std::string,std::string>, PluginSpecHash, PluginSpecName> data;

	std::vector<std::string> listAllPlugins() const;
	std::string lookupInfo(PluginSpec const & spec, std::string const & which) const;
};

}

}

#endif

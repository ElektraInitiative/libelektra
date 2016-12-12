/**
 * @file
 *
 * @brief Interface to all plugins
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */


#ifndef TOOLS_PLUGIN_DATABASE_HPP
#define TOOLS_PLUGIN_DATABASE_HPP

#include <map>
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
protected:
	typedef void (*func_t) ();

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
	virtual std::vector<std::string> listAllPlugins () const = 0;

	// TODO: add additional functions:
	// virtual void registerPlugin(PluginSpec) = 0;
	// virtual std::vector<PluginSpec> listAllPlugins() const = 0;

	enum Status
	{
		/// does not directly, but can be loaded via provides
		provides,
		/// exists and working as given
		real,
		/// does not exist or cannot be loaded
		missing
	};

	virtual Status status (PluginSpec const & whichplugin) const = 0;

	/**
	 * @brief lookup contract clauses or dynamic information
	 *
	 * @param whichplugin about which plugin?
	 * @param which about which clause in the contract?
	 *
	 * @return the clause of the contract
	 */
	virtual std::string lookupInfo (PluginSpec const & whichplugin, std::string const & which) const = 0;

	/**
	 * @brief get exported plugin symbol
	 *
	 * @param whichplugin from which plugin?
	 * @param which which symbol would you like to look up?
	 *
	 * @return the function pointer to the exported symbol or NULL if the symbol was not found
	 */
	virtual func_t getSymbol (PluginSpec const & whichplugin, std::string const & which) const = 0;

	/**
	 * @brief lookup which plugin handles metadata
	 *
	 * @param which the metadata of interest
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

	/**
	 * @brief looks up all plugins which are a suitable provider
	 *
	 * @note in case a plugin name is provided, the plugin with the name will also be
	 *	 part of the result. But if there are other plugins providing the requirement,
	 *	 then they will also be part of the result.
	 *
	 * @param provides is the provider to find
	 *
	 * @return a map of plugins with their status offering the requirement or are named after it
	 */
	virtual std::map<int, PluginSpec> lookupAllProvidesWithStatus (std::string const & provides) const = 0;

	/**
	 * @brief looks up all plugins which are a suitable provider
	 *
	 * @note in case a plugin name is provided, the plugin with the name will also be
	 *	 part of the result. But if there are other plugins providing the requirement,
	 *	 then they will also be part of the result.
	 *       The ordering of the resulting vector has no special meaning.
	 *
	 * @param provides is the provider to find
	 *
	 * @return a vector of plugins offering the requirement or are named after it
	 */
	virtual std::vector<PluginSpec> lookupAllProvides (std::string const & provides) const = 0;

	/**
	 * @param statusString the string encoding the status
	 *
	 * @return The representing number for a given status.
	 */
	static int calculateStatus (std::string statusString);

	static const std::map<std::string, int> statusMap;
};

typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;

/**
 * @brief A plugin database that works with installed modules
 */
class ModulesPluginDatabase : public PluginDatabase
{
	class Impl;
	std::unique_ptr<Impl> impl;

public:
	ModulesPluginDatabase ();
	~ModulesPluginDatabase ();

	std::vector<std::string> listAllPlugins () const;
	PluginDatabase::Status status (PluginSpec const & whichplugin) const;
	std::string lookupInfo (PluginSpec const & spec, std::string const & which) const;
	func_t getSymbol (PluginSpec const & whichplugin, std::string const & which) const;
	PluginSpec lookupMetadata (std::string const & which) const;
	PluginSpec lookupProvides (std::string const & provides) const;
	std::map<int, PluginSpec> lookupAllProvidesWithStatus (std::string const & provides) const;
	std::vector<PluginSpec> lookupAllProvides (std::string const & provides) const;
};

class PluginVariantsDatabase : public ModulesPluginDatabase
{
	class Impl;
	std::unique_ptr<Impl> impl;

public:
	/**
	 * @brief constructor that takes a configuration keyset for plugins
	 * 
	 * takes the list of plugins provided by the ModulesPluginDatabase and
	 * removes all plugins that are disabled in the system config which was
	 * given to the constructor.
	 * 
	 * example: removes the `simpleini` plugin if an entry like
	 * 
	 *   system/elektra/plugins/simpleini/disable = 1
	 * 
	 * exists in the keyset handed to the constructor
	 *  
	 * @note the constructor should be called with a keyset containing
	 * the keys for system/elektra/plugins
	 * 
	 * @param conf keyset containing keys from system/elektra/plugins
	 */
	PluginVariantsDatabase (const KeySet & conf);
	~PluginVariantsDatabase ();

	std::vector<std::string> listAllPlugins () const;

	/**
	 * @brief returns a list of plugin variants for the plugin
	 * 
	 * takes the genconf function provided by plugins to generate variants.
	 * variants can be disabled through the system configuration handed to
	 * the constructor of the class.
	 * 
	 * example: ignores a variant `spacesep` delivered by genconf if an entry like
	 * 
	 *   system/elektra/plugins/simpleini/variants/spacesep/disable = 1
	 * 
	 * exists in the keyset handed to the constructor
	 *
	 * @note if no plugin variant could be found, an empty vector will be returned.
	 *
	 * @param whichplugin is the plugin for which we want all variants
	 *
	 * @return a vector of plugin variants for the given plugin
	 */
	std::vector<PluginSpec> getPluginVariants (PluginSpec const & whichplugin) const;

private:
	KeySet pluginconf;
};

/**
 * @brief A plugin database that works with added fake data
 */
class MockPluginDatabase : public ModulesPluginDatabase
{
public:
	typedef int (*checkConfPtr) (ckdb::Key *, ckdb::KeySet *);

	/// only data from here will be returned
	/// @note that it is ordered by name, i.e., different ref-names cannot be distinguished
	mutable std::unordered_map<PluginSpec, std::unordered_map<std::string, std::string>, PluginSpecHash, PluginSpecName> data;

	std::vector<std::string> listAllPlugins () const;
	PluginDatabase::Status status (PluginSpec const & whichplugin) const;
	std::string lookupInfo (PluginSpec const & spec, std::string const & which) const;
	func_t getSymbol (PluginSpec const & whichplugin, std::string const & which) const;
	void setCheckconfFunction (checkConfPtr const newCheckconf);

private:
	checkConfPtr checkconf = NULL;
};
}
}

#endif

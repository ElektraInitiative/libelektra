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

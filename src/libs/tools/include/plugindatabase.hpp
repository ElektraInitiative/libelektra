/**
 * @file
 *
 * @brief Interface to all plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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
	/* TODO: reintroduce with next API break
	virtual ~PluginDatabase ()
	{
	}
	*/

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
protected:
	class Impl;
	std::unique_ptr<Impl> impl;

public:
	ModulesPluginDatabase ();
	~ModulesPluginDatabase ();
	/* TODO: reintroduce with next API break
	virtual ~ModulesPluginDatabase ();
	*/

	std::vector<std::string> listAllPlugins () const;
	PluginDatabase::Status status (PluginSpec const & whichplugin) const;
	std::string lookupInfo (PluginSpec const & spec, std::string const & which) const;
	func_t getSymbol (PluginSpec const & whichplugin, std::string const & which) const;
	PluginSpec lookupMetadata (std::string const & which) const;
	PluginSpec lookupProvides (std::string const & provides) const;
	std::map<int, PluginSpec> lookupAllProvidesWithStatus (std::string const & provides) const;
	std::vector<PluginSpec> lookupAllProvides (std::string const & provides) const;
};

class PluginVariantDatabase : public ModulesPluginDatabase
{
	class VariantImpl;
	std::unique_ptr<VariantImpl> variantImpl;

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
	 *   system:/elektra/plugins/simpleini/disable = 1
	 *
	 * exists in the keyset handed to the constructor
	 *
	 * @note the constructor should be called with a keyset containing
	 * the keys for system:/elektra/plugins
	 *
	 * @param conf keyset containing keys from system:/elektra/plugins
	 */
	explicit PluginVariantDatabase (const KeySet & conf);
	~PluginVariantDatabase ();
	/* TODO: reintroduce with next API break
	virtual ~PluginVariantDatabase ();
	*/

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
	 *   system:/elektra/plugins/simpleini/variants/spacesep/disable = 1
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
	/**
	 * @brief returns a list of plugin variants created from the system config
	 *
	 * considered are keys below system:/elektra/plugins/<plugin>/variants
	 *
	 * @note variants listed in @p genconfToIgnore are not added to the result.
	 * it is expected that they were added at another point already
	 * (e.g. explicit override check).
	 *
	 * @param whichplugin is the plugin for which we want all variants
	 * @param sysconf is a keyset containing the system config for system:/elektra/plugins
	 * @param genconfToIgnore is a keyset containing variants to ignore from the sysconf
	 *
	 * @return a vector of pluginspecs with variant configurations
	 */
	std::vector<PluginSpec> getPluginVariantsFromSysconf (PluginSpec const & whichplugin, KeySet const & sysconf,
							      KeySet const & genconfToIgnore) const;

	/**
	 * @brief returns a list of plugin variants created from the genconf config
	 *
	 * does take a keyset with config from the `genconf` plugin function, but also
	 * an additional @p sysconf keyset with config from system:/elektra/plugins to
	 * ensure overrides and disabled variants.
	 *
	 * the function does also add all variants from @p sysconf that were not mentioned
	 * in @p genconf yet.
	 *
	 * @param whichplugin is the plugin for which we want all variants
	 * @param genconf is a keyset containing the genconf config from the plugin
	 * @param sysconf is a keyset containing the system config for system:/elektra/plugins
	 *
	 * @return a vector of pluginspecs with variant configurations
	 */
	std::vector<PluginSpec> getPluginVariantsFromGenconf (PluginSpec const & whichplugin, KeySet const & genconf,
							      KeySet const & sysconf) const;

	/**
	 * @brief builds a sysconf key from several inputs
	 *
	 * builds a key like:
	 *
	 *   system:/elektra/plugin/<whichplugin>/variants/<variant>/<attr>
	 *
	 * @note the function does not add a value and it does not lookup the key in any
	 * keyset, it just creates the key by adding every part as basename.
	 *
	 * @param whichplugin is the plugin for which we want a key
	 * @param variant is the plugin variant for which we want a key
	 * @param attr is the attribute of a variant for which we want a key, e.g. info or config
	 *
	 * @return a newly created key matching the inputs which can be used for lookups for example
	 */
	Key buildVariantSysconfKey (PluginSpec const & whichplugin, std::string const & variant, const std::string attr) const;

	/**
	 * @brief adds all keys of a keyset below a certain key to another keyset, rebased
	 *
	 * lets take the input keyset (@p conf):
	 *
	 *   system:/elektra/plugins/simpleini/variants/spacesep
	 *   system:/elektra/plugins/simpleini/variants/spacesep/config
	 *   system:/elektra/plugins/simpleini/variants/spacesep/config/format = % %
	 *   system:/elektra/plugins/simpleini/variants/spacesep/config/ignorewhitespace = 1
	 *
	 * and the input key (@p below):
	 *
	 *   system:/elektra/plugins/simpleini/variants/spacesep/config
	 *
	 * and the new base key (@p newbase):
	 *
	 *   system:/
	 *
	 * then we get the following keys in the output keyset (@p targetconf):
	 *
	 *   system:/format = % %
	 *   system:/ignorewhitespace = 1
	 *
	 * @param below the parent key for everything we want to add to the target keyset
	 * @param conf the keyset of which we want to add the keys to the target keyset
	 * @param newbase the new base key used in the rebasing process
	 * @param targetconf the target keyset to use for the transformation
	 */
	void addKeysBelowKeyToConf (Key const & below, KeySet const & conf, Key const & newbase, KeySet & targetconf) const;
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


typedef std::shared_ptr<PluginDatabase> PluginDatabasePtr;
} // namespace tools
} // namespace kdb

#endif

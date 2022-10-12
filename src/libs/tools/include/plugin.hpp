/**
 * @file
 *
 * @brief Header file of plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef TOOLS_PLUGIN_HPP
#define TOOLS_PLUGIN_HPP

#include <kdb.hpp>
#include <pluginspec.hpp>
#include <toolexcept.hpp>

#include <map>
#include <string>
#include <vector>

namespace ckdb
{
typedef struct _Plugin Plugin;
}

namespace kdb
{

namespace tools
{

/**
 * This is a C++ representation of a plugin.
 *
 * It will load an Elektra plugin using the module loader
 * from Elektra.
 *
 * Then you can either check the plugins configuration
 * using loadInfo(), parse() and check.
 * Symbols can then be retrieved with getSymbol().
 *
 * Or you can use the normal open(), close(), get(),
 * set() and error() API which every plugin exports.
 */
class Plugin
{
private:
	typedef void (*func_t) ();

private:
	ckdb::Plugin * plugin;
	PluginSpec spec;
	kdb::KeySet info;

	std::map<std::string, func_t> symbols;
	std::map<std::string, std::string> infos;

	void uninit ();

public:
	/**
	 * @brief Do not construct a plugin yourself, use Modules.load
	 */
	Plugin (PluginSpec const & spec, kdb::KeySet & modules);

	Plugin (Plugin const & other);
	Plugin & operator= (Plugin const & other);

	Plugin (Plugin && other) = delete;
	Plugin & operator= (Plugin && other) = delete;

	~Plugin ();

	/**
	 * @brief Is toggled during serialization.
	 *
	 * (is a hack, only allows a single serialization!)
	 */
	bool firstRef;

	/**
	 * Gets the configuration for the plugin.
	 * (will be done in Modules.load)
	 */
	void loadInfo ();

	/**
	 * Creates symbol and info table.
	 * (will be done in Modules.load)
	 */
	void parse ();

	/**
	 * Does various checks on the Plugin and throws exceptions
	 * if something is not ok.
	 *
	 * - Check if Plugin is compatible to current Version of Backend-API.
	 *
	 * @throw PluginCheckException if there are errors
	 * @param warnings for warnings
	 *
	 * @pre parse()
	 */
	void check (std::vector<std::string> & warnings);

	ckdb::Plugin * operator-> ();

	/**
	 * Gets the whole string of an information item.
	 * @pre loadInfo()
	 */
	std::string lookupInfo (std::string item, std::string section = "infos");

	/**
	 * Searches within a string of an information item.
	 * @pre loadInfo()
	 */
	bool findInfo (std::string check, std::string item, std::string section = "infos");

	/**
	 * Returns the whole keyset of information.
	 * @pre loadInfo()
	 */
	kdb::KeySet getInfo ()
	{
		return info;
	}

	/**
	 * In the plugin's contract there is a description of which
	 * config is needed in order to work together with a backend
	 * properly.
	 *
	 * @return the keyset with the config needed for the backend.
	 * @see getConfig()
	 * @pre loadInfo()
	 */
	kdb::KeySet getNeededConfig ();

	/**
	 * @brief return the plugin config
	 *
	 * @return the config supplied with constructor
	 * @see getNeededConfig()
	 */
	kdb::KeySet getConfig ();

	/**
	 * Returns symbol to a function.
	 * @pre parse()
	 */
	func_t getSymbol (std::string which)
	{
		if (symbols.find (which) == symbols.end ()) throw MissingSymbol (which, name ());
		return symbols[which];
	}

	/**
	 * Calls the open function of the plugin
	 * @pre parse()
	 */
	int open (kdb::Key & errorKey);

	/**
	 * Calls the close function of the plugin
	 * @pre parse()
	 */
	int close (kdb::Key & errorKey);

	/**
	 * Calls the get function of the plugin
	 * @pre parse()
	 */
	int get (kdb::KeySet & ks, kdb::Key & parentKey);

	/**
	 * Calls the set function of the plugin
	 * @pre parse()
	 */
	int set (kdb::KeySet & ks, kdb::Key & parentKey);

	/**
	 * Calls the commit function of the plugin
	 * @pre parse()
	 */
	int commit (kdb::KeySet & ks, kdb::Key & parentKey);

	/**
	 * Calls the error function of the plugin
	 * @pre parse()
	 */
	int error (kdb::KeySet & ks, kdb::Key & parentKey);

	/**
	 * @return the name of the plugin (module)
	 */
	std::string name ();

	/**
	 * @return the fullname of the plugin
	 */
	std::string getFullName ();

private:
	friend class CommitPlugins;
	friend class ErrorPlugins;
	friend class GetPlugins;
	friend class SetPlugins;
	/**
	 * @return the name how it would be referred to in mountpoint
	 *
	 * # name # label # (when called the first time)
	 * or
	 * # ref
	 *
	 * @note Its not the same as getRefName(), and is only suitable for serialization!
	 * @warning Its stateful in a really weird way!
	 */
	std::string refname ();
};

typedef std::unique_ptr<Plugin> PluginPtr;
} // namespace tools
} // namespace kdb

#endif

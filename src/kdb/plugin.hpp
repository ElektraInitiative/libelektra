#ifndef PLUGIN_HPP
#define PLUGIN_HPP

#include <kdb.hpp>
#include <command.hpp>

#include <map>
#include <vector>
#include <string>

namespace ckdb
{
	typedef struct _Plugin Plugin;
}

struct PluginCheckException : public CommandException
{
	virtual const char* what() const throw()
	{
		return  "When you read this, that means there was something wrong with the plugin.\n"
			"Seems like a check could not specify the error any further";
	}
};

struct NoPlugin : public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return  "Was not able to load such a plugin!\n\n"
			"Maybe you misspelled it, there is no such plugin or the loader has problems.\n"
			"You might want to try to set LD_LIBRARY_PATH to /usr/lib/elektra.";
	}
};

struct ReferenceNotFound: public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return  "Could not find a reference!\n"
			"Seems you forgot to create the reference before using it.\n"
			"Use #modulename#label# before you #ref to it.";
	}
};

struct BadPluginName : public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return  "You entered a bad name for a plugin!\n"
			"A valid name of a plugin has either no #\n"
			"or of the following form: #modulename#label# or #ref\n"
			"where ref must be one of the previously defined labels";
	}
};

struct MissingNeeded : public PluginCheckException
{
	std::string need;
	MissingNeeded (std::string const& need_) :
		need(need_)
	{}
	~MissingNeeded () throw()
	{}
	virtual const char* what() const throw()
	{
		return std::string(std::string("The plugin ") + need + " is needed by this plugin but it is not provided.").c_str();
	}
};

struct MissingSymbol: public PluginCheckException
{
	std::string symbol;
	MissingSymbol (std::string const& symbol_) :
		symbol(symbol_)
	{}
	~MissingSymbol () throw()
	{}
	virtual const char* what() const throw()
	{
		// TODO: not safe return value
		return std::string(std::string("The necessary symbol \"") + symbol + "\" is missing in that plugin!").c_str();
	}
};

struct SymbolMismatch: public PluginCheckException
{
	std::string symbol;
	SymbolMismatch (std::string const& symbol_) :
		symbol(symbol_)
	{}
	~SymbolMismatch () throw()
	{}
	virtual const char* what() const throw()
	{
		// TODO: not safe return value
		return std::string(std::string("The symbol \"") + symbol + "\" does not match with other exported information!").c_str();
	}
};

struct StoragePlugin : public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return "There need to be exactly one storage plugin!";
	}
};


struct ResolverPlugin : public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return "There need to be exactly one resolver plugin!";
	}
};

struct PluginWrongName : public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return "The real name of the plugin is different!";
	}
};

struct PluginNoInfo: public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return "No info found for that plugin!";
	}
};

struct VersionInfoMismatch: public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return "Version info does not match with library!";
	}
};



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
	typedef void (*func_t)();

private:
	ckdb::Plugin *plugin;
	std::string pluginName;
	kdb::KeySet info;

	std::map<std::string, func_t> symbols;
	std::map<std::string, std::string> infos;

	bool firstRef;

	void uninit();

public:
	Plugin(std::string const& pluginName, kdb::KeySet &modules, kdb::KeySet const& testConfig);

	Plugin(Plugin const& other);
	Plugin& operator = (Plugin const& other);
	~Plugin();

	/**
	 * Gets the configuration for the plugin.
	 */
	void loadInfo();

	/**
	 * Creates symbol and info table.
	 */
	void parse();

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
	void check(std::vector<std::string> & warnings);

	ckdb::Plugin *operator->();

	/**
	 * Gets the whole string of an information item.
	 * @pre loadInfo()
	 */
	std::string lookupInfo(std::string item, std::string section = "infos");

	/**
	 * Searches within a string of an information item.
	 * @pre loadInfo()
	 */
	bool findInfo(std::string check, std::string item, std::string section = "infos");

	/**
	 * Returns the whole keyset of information.
	 * @pre loadInfo()
	 */
	kdb::KeySet getInfo() {return info;}

	/**
	 * In the plugin's contract there is a description of which
	 * config is needed in order to work together with a backend
	 * properly.
	 *
	 * @return the keyset with the config needed for the backend.
	 * @pre loadInfo()
	 */
	kdb::KeySet getNeededConfig();

	/**
	 * Returns symbol to a function.
	 * @pre parse()
	 */
	func_t getSymbol (std::string which)
	{
		if (symbols.find (which) == symbols.end()) throw MissingSymbol(which);
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
	 * Calls the error function of the plugin
	 * @pre parse()
	 */
	int error (kdb::KeySet & ks, kdb::Key & parentKey);

	/**
	 * Not working well (except for dump).
	 * Do not use.
	 *
	 * It tries to load and call some serialize functions.
	 */
	void serialize (kdb::KeySet & ks);

	/**
	 * Not working well (except for dump).
	 * Do not use.
	 *
	 * It tries to load and call some unserialize functions.
	 */
	void unserialize (kdb::KeySet & ks);

	/**
	 * @return the name of the plugin 
	 */
	std::string name();

	/**
	 * @return the name how it would be referred to in mountpoint
	 */
	std::string refname();
};

#endif

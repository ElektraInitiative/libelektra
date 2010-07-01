#ifndef PLUGIN_HPP
#define PLUGIN_HPP

#include <kdb>
#include <command.hpp>

#include <map>
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
		return  "Was not able to load such a plugin!\n"
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
	MissingNeeded (std::string const& need) :
		need(need)
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
	MissingSymbol (std::string const& symbol) :
		symbol(symbol)
	{}
	~MissingSymbol () throw()
	{}
	virtual const char* what() const throw()
	{
		return std::string(std::string("The necessary symbol ") + symbol + " is missing in that plugin!").c_str();
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


class Plugin
{
private:
	ckdb::Plugin *plugin;
	std::string pluginName;
	kdb::KeySet info;

	typedef void (*func_t)();
	std::map<std::string, func_t> symbols;
	std::map<std::string, std::string> infos;

	bool firstRef;

	void close();

public:
	Plugin(std::string const& pluginName, kdb::KeySet &modules, kdb::KeySet const& testConfig);

	Plugin(Plugin const& other);
	Plugin& operator = (Plugin const& other);
	~Plugin();

	void parse();

	ckdb::Plugin *operator->();
	bool operator!();
	std::string lookupInfo(std::string item, std::string section = "infos");
	kdb::KeySet getInfo() {return info;}
	func_t getSymbol (std::string which) {return symbols[which];}

	std::string refname();
};

#endif

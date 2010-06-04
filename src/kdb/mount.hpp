#ifndef MOUNT_HPP
#define MOUNT_HPP

#include <command.hpp>
#include <kdb>

struct NameAlreadyInUseException : public CommandException
{
	virtual const char* what() const throw()
	{
		return "Name already used, will abort";
	}
};

struct MountpointAlreadyInUseException : public CommandException
{
	virtual const char* what() const throw()
	{
		return "Mountpoint already used, will abort";
	}
};

struct MountpointInvalidException : public CommandException
{
	virtual const char* what() const throw()
	{
		return  "Given mountpoint is not a valid keyname, will abort\n"
			"Examples: system/hosts or user/sw/app";
	}
};

struct PathInvalidException : public CommandException
{
	virtual const char* what() const throw()
	{
		return  "Given path could not be opened\n"
			"You must provide a valid file name for the global path";
	}
};

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
		return  "Could not found a reference!\n"
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



class MountCommand : public Command
{
	kdb::KDB kdb;
	static std::string root;
public:
	MountCommand();
	kdb::KeySet addPlugins(std::string name, kdb::KeySet& referencePlugins, std::string which);
	bool checkFile(std::string file);
	int execute(int argc, char** argv);
	~MountCommand();
};

#endif

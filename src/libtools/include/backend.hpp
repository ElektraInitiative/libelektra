#ifndef BACKEND_HPP
#define BACKEND_HPP

#include <plugins.hpp>

#include <string>
#include <stdexcept>

#include <kdb.hpp>


struct BackendCheckException : public std::exception
{
	virtual const char* what() const throw()
	{
		return  "When you read this, that means there was something wrong with the backend.\n"
			"Seems like a check could not specify the error any further";
	}
};

struct FileNotValidException : public BackendCheckException
{
	virtual const char* what() const throw()
	{
		return  "The path/mountpoint combination does not work this way.\n"
			"Try to add another path or mountpoint instead.\n"
			"\n"
			"Filenames for cascading and user mountpoints\n"
			"are not allowed to be absolute (starting with /),\n"
			"because user configuration files are by definition\n"
			"different files per-user.\n"
			"\n"
			"If you want to mount an absolute filename, mount\n"
			"it into system/ regardless if it is /etc or somewhere\n"
			"else. Note that the file permissions will apply, so\n"
			"it might be possible for non-root to modify this path\n"
			"of the system-hierarchy.\n"
			;
	}
};

struct PluginAlreadyInserted: public PluginCheckException
{
	virtual const char* what() const throw()
	{
		return  "It is not allowed to insert the same plugin again!\n"
			"Try to add other plugins instead.";
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

struct MountpointInvalidException : public BackendCheckException
{
	virtual const char* what() const throw()
	{
		return  "Given mountpoint is not a valid keyname, will abort\n"
			"Examples: system/hosts or user/sw/app";
	}
};

class Backend
{
private:
	GetPlugins getplugins;
	SetPlugins setplugins;
	ErrorPlugins errorplugins;

	std::string name;
	std::string mp;

	kdb::KeySet modules;
	kdb::KeySet config; // the global config, plugins might add something to it

	std::vector <Plugin*> plugins;

public:
	Backend(std::string name, std::string mountpoint);
	~Backend();

	void checkFile (std::string file);
	void tryPlugin (std::string name);
	void addPlugin ();
	bool validated ();
	void serialize (kdb::Key &rootKey, kdb::KeySet &ret);
};

#endif

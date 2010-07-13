#ifndef BACKEND_HPP
#define BACKEND_HPP

#include <plugins.hpp>

#include <string>

#include <kdb>


struct BackendCheckException : public CommandException
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
		return  "The path you entered does not present a valid file!\n"
			"Try to add another path instead.";
	}
};

struct PluginAlreadyInserted: public BackendCheckException
{
	virtual const char* what() const throw()
	{
		return  "It is not allowed to insert the same plugin again!\n"
			"Try to add other plugins instead.";
	}
};

class Backend
{
private:
	GetPlugins getplugins;
	SetPlugins setplugins;
	ErrorPlugins errorplugins;

	std::string name;

	kdb::KeySet modules;

	std::vector <Plugin*> plugins;

public:
	Backend(std::string name);
	~Backend();

	/**
	 * If the resolver was loaded first, this will check the filename */
	void checkFile (std::string file);
	void tryPlugin (std::string name);
	void addPlugin ();
	bool validated ();
	void serialize (kdb::Key &rootKey, kdb::KeySet &ret);
};

#endif

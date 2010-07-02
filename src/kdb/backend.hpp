#ifndef BACKEND_HPP
#define BACKEND_HPP

#include <plugins.hpp>

#include <string>

#include <kdb>

struct PluginAlreadyInserted: public PluginCheckException
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

	void tryPlugin (std::string name);
	void addPlugin ();
	bool validated ();
	void serialize (kdb::Key &rootKey, kdb::KeySet &ret);
};

#endif

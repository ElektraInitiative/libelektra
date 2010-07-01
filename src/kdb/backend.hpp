#ifndef BACKEND_HPP
#define BACKEND_HPP

#include <plugins.hpp>

#include <string>

#include <kdb>

class Backend
{
private:
	GetPlugins getplugins;
	SetPlugins setplugins;
	ErrorPlugins errorplugins;

	std::string name;

	kdb::KeySet modules;

	std::vector <Plugin> plugins;

public:
	Backend(std::string name);
	~Backend();

	void addPlugin (std::string name);
	void serialize (kdb::Key &rootKey, kdb::KeySet &ret);
};

#endif

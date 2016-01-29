/**
 * @file
 *
 * @brief source file of mount command
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <mount.hpp>
#include <backend.hpp>
#include <backends.hpp>
#include <cmdline.hpp>
#include <backendbuilder.hpp>

#include <iostream>
#include <iterator>
#include <fstream>
#include <vector>
#include <string>

#include <kdbprivate.h>
#include <kdbmodule.h>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

class KDBMountException : public KDBException
{
	std::string msg;
public:
	KDBMountException(std::string const & e) :
		KDBException (Key())
	{
		msg = e;
	}

	virtual const char* what() const noexcept override
	{
		return msg.c_str();
	}
};

MountCommand::MountCommand()
{}


/**
 * @brief Output what currently is mounted
 */
void MountCommand::outputMtab(Cmdline const& cl)
{
	Backends::BackendInfoVector mtab = Backends::getBackendInfo(mountConf);
	bool all = cl.first && cl.second && cl.third;
	char delim = '\n';
	if (cl.null) delim = '\0';

	for (Backends::BackendInfoVector::const_iterator it=mtab.begin();
			it!=mtab.end(); ++it)
	{
		if (cl.first)
		{
			std::cout << it->path;
			if (all) std::cout << " on ";
			else std::cout << delim << std::flush;
		}

		if (cl.second)
		{
			std::cout << it->mountpoint;
			if (all) std::cout << " with name ";
			else std::cout << delim << std::flush;
		}

		// TODO: remove next version
		if (cl.third)
		{
			std::cout << it->name;
			std::cout << delim << std::flush;
		}
	}
}

void MountCommand::processArguments(Cmdline const& cl)
{
	if (!cl.interactive && cl.arguments.size() == 1)
	{
		throw invalid_argument("wrong number of arguments, 0 or more then 1 needed");
	}

	if (cl.interactive)
	{
		cout << "Welcome to interactive mounting" << endl;
		cout << "Note that nothing will be made persistent" << endl;
		cout << "until you say y at the very end of the mounting process" << endl;
		cout << endl;
		cout << "Please provide a unique name." << endl;
	}
}

void MountCommand::buildBackend(Cmdline const& cl)
{
	MountBackendBuilder backend;

	Key mpk(mp, KEY_CASCADING_NAME, KEY_END);

	if (!mpk.isValid())
	{
		throw invalid_argument(mp + " is not a valid mountpoint");
	}

	backend.setMountpoint(mpk, mountConf);

	backend.setBackendConfig(cl.getPluginsConfig("system/"));

	PluginSpec resolver (cl.resolver);
	if (cl.debug)
	{
		cout << "Trying to load the resolver plugin " << resolver.getName() << endl;
	}

	backend.addPlugin (PluginSpec(resolver));

	if (cl.interactive)
	{
		cout << endl;
		cout << "Enter a path to a file in the filesystem." << endl;
		cout << "The path must either not exist or be a file." << endl;
		cout << "For user or cascading mountpoints it must be a relative path." << endl;
		cout << "Then, the path will be resolved dynamically." << endl;
		cout << "Path: ";
		cin >> path;
	}
	else
	{
		path = cl.arguments[0];
	}

	backend.useConfigFile(path);

	istringstream sstream(cl.plugins);
	std::string plugin;
	while (std::getline (sstream, plugin, ' '))
	{
		if (cl.debug)
		{
			cout << "Trying to add default plugin " << plugin << endl;
		}
		backend.addPlugin (PluginSpec(plugin));
	}


	if (cl.interactive)
	{
		cout << "Now enter a sequence of plugins you want in the backend" << endl;
		appendPlugins(backend);
	}
	else
	{
		const int alreadyRead = 2;
		if (cl.arguments.size() <= alreadyRead)
		{
			backend.addPlugin(PluginSpec(KDB_DEFAULT_STORAGE));
		}
		else
		{
			// TODO: virtual inheritance
			// backend.appendPluginsByArguments(cl.arguments.begin()+alreadyRead, cl.arguments.end());
			// dynamic_cast<MountBackendInterface&>(backend).appendPluginsByArguments(cl.arguments.begin()+alreadyRead, cl.arguments.end());
			backend.MountBackendInterface::appendPluginsByArguments(cl.arguments.begin()+alreadyRead, cl.arguments.end());
		}
	}

	// Call it a day
	backend.resolveNeeds();
	backend.serialize (mountConf);
}

void MountCommand::readPluginConfig (KeySet & pluginConfig)
{
	string keyName;
	string value;

	cout << "Enter the plugin configuration" << endl;
	cout << "Use '.' as Key name to finish" << endl;

	while (true)
	{
		cout << "Enter the Key name: ";
		cin >> keyName;

		if (keyName == ".") break;

		cout << "Enter the Key value: ";
		cin >> value;

		pluginConfig.append(Key("user/"+keyName, KEY_VALUE, value.c_str(), KEY_END));
	}
}

void MountCommand::appendPlugins (MountBackendInterface & backend)
{
	std::string pname;
	KeySet pluginConfig;
	cout << "First Plugin: ";
	cin >> pname;
	readPluginConfig (pluginConfig);

	while (pname != "." || !backend.validated())
	{
		try {
			backend.addPlugin (PluginSpec(pname, pluginConfig));
			pluginConfig.clear();
		}
		catch (PluginCheckException const& e)
		{
			cerr << "Could not add that plugin" << endl;
			cerr << e.what() << endl;
		}
		if (!backend.validated()) cout << "Not validated, try to add another plugin (. to abort)" << endl;
		else cout << "Enter . to finish entering plugins" << endl;

		cout << endl;
		cout << "Next Plugin: ";
		cin >> pname;

		if (pname != ".")
		{
			readPluginConfig (pluginConfig);
		}

		if (pname == "." && !backend.validated())
		{
			std::ostringstream os;
			backend.status(os);
			throw CommandAbortException(os.str().c_str());
		}
	}

}


/**
 * @brief Its quite linear whats going on, but there are many steps involved
 *
 * @param cl the commandline
 *
 * @retval 0 on success (otherwise exception)
 */
int MountCommand::execute(Cmdline const& cl)
{
	readMountConf(cl);

	if (!cl.interactive && cl.arguments.empty())
	{
		// no interactive mode, so lets output the mtab
		outputMtab(cl);
		return 0;
	}

	processArguments(cl);
	getMountpoint(cl);
	buildBackend(cl);
	askForConfirmation(cl);
	try {
		doIt();
	}
	catch (KDBException const & e)
	{
		throw KDBMountException(std::string(e.what())+"\n\n"
				"IMPORTANT: Make sure you can write to system namespace\n"
				"           Usually you need to be root for that!");
	}

	return 0;
}

MountCommand::~MountCommand()
{}

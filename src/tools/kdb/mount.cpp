/**
 * \file
 *
 * \brief source file of mount command
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <mount.hpp>
#include <backend.hpp>
#include <backends.hpp>
#include <cmdline.hpp>

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

MountCommand::MountCommand()
{}


/**
 * @brief Output what currently is mounted
 */
void MountCommand::outputMtab()
{
	Backends::BackendInfoVector mtab = Backends::getBackendInfo(mountConf);

	for (Backends::BackendInfoVector::const_iterator it=mtab.begin();
			it!=mtab.end(); ++it)
	{
		std::cout << it->path << " on " << it->mountpoint
			  << " with name " << it->name << std::endl;
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
	Backend backend (name, mp);

	if (cl.debug)
	{
		cout << "Trying to load the resolver plugin " << cl.resolver << endl;
	}

	backend.addPlugin (cl.resolver);


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

	backend.checkFile (path);

	std::string configPath = Backends::getConfigBasePath(name);

	mountConf.append ( *Key(configPath,
			KEY_VALUE, "",
			KEY_COMMENT, "This is a configuration for a backend, see subkeys for more information",
			KEY_END));
	configPath += "/path";
	mountConf.append ( *Key(configPath,
			KEY_VALUE, path.c_str(),
			KEY_COMMENT, "The path for this backend. Note that plugins can override that with more specific configuration.",
			KEY_END));


	if (cl.interactive)
	{
		cout << "Now enter a sequence of plugins you want in the backend" << endl;
	}

	appendPlugins(cl, backend);

	Key rootKey (Backends::mountpointsPath, KEY_END);
	backend.serialise (rootKey, mountConf);
}

void MountCommand::addConfig (string const& configBasePath, string const& keyName, string const& value)
{
	Key configKey = Key (configBasePath + "/" + keyName, KEY_END);
	// configKey.addName (keyName);
	configKey.setString (value);
	mountConf.append (configKey);
}

bool MountCommand::readPluginConfig(Cmdline const& cl, size_t current_token)
{
	string keyName;
	string value;

	const string configBasePath = Backends::getConfigBasePath (name);
	if (cl.interactive)
	{
		cout << "Enter the plugin configuration" << endl;
		cout << "Use '.' as Key name to finish" << endl;

		while (true)
		{
			cout << "Enter the Key name: ";
			cin >> keyName;

			if (keyName == ".") break;

			cout << "Enter the Key value: ";
			cin >> value;

			addConfig (configBasePath, keyName, value);
		}
	}
	else
	{
		// check if there is a further token (which might be the config of the current plugin)
		if (current_token + 1 >= cl.arguments.size ()) return false;

		string configString = cl.arguments[current_token + 1];

		// check if the next argument is a config (otherwise it is treated as plugin name)
		// only a token with a '=' is treated as config
		if (configString.find ('=') == string::npos) return false;

		istringstream sstream(configString);

		while (true)
		{
			// read until the next '=', this will be the keyname
			if (!std::getline (sstream, keyName, '=')) break;

			// read until a ',' or the end of line
			// if nothing is read because the '=' is the last character
			// in the config string, consider the value empty
			if (!std::getline (sstream, value, ',')) value = "";

			addConfig (configBasePath, keyName, value);
		}
	}

	return true;
}

void MountCommand::appendPlugins(Cmdline const& cl, Backend & backend)
{
	std::string pname;
	size_t current_plugin = 2;
	if (cl.interactive)
	{
		cout << "First Plugin: ";
		cin >> pname;
		readPluginConfig (cl, current_plugin);
	}
	else
	{
		if (current_plugin >=  cl.arguments.size())
		{
			pname = "dump";
		}
		else
		{
			pname = cl.arguments[current_plugin];
			if (readPluginConfig (cl, current_plugin))
			{
				current_plugin ++;
			}
		}
		current_plugin ++;
	}


	while (pname != "." || !backend.validated())
	{
		try {
			backend.addPlugin (pname);
		}
		catch (PluginCheckException const& e)
		{
			cerr << "Could not add that plugin" << endl;
			cerr << e.what() << endl;
		}
		if (cl.interactive)
		{
			if (!backend.validated()) cout << "Not validated, try to add another plugin (. to abort)" << endl;
			else cout << "Enter . to finish entering plugins" << endl;
		}

		if (cl.interactive)
		{
			cout << endl;
			cout << "Next Plugin: ";
			cin >> pname;

			if (pname != ".")
			{
				readPluginConfig (cl, current_plugin);
			}
		}
		else
		{
			if (current_plugin >=  cl.arguments.size())
			{
				pname = ".";
			}
			else
			{
				pname = cl.arguments[current_plugin];
				if (readPluginConfig (cl, current_plugin))
				{
					current_plugin ++;
				}
			}
			current_plugin ++;
		}

		if (pname == "." && !backend.validated())
		{
			std::cerr << backend << std::endl;
			throw CommandAbortException();
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
	readMountConf();

	if (!cl.interactive && cl.arguments.empty())
	{
		// no interactive mode, so lets output the mtab
		outputMtab();
		return 0;
	}

	processArguments(cl);
	fixRootKey(cl);
	getName(cl);
	getMountpoint(cl);
	buildBackend(cl);
	askForConfirmation(cl);
	doIt();

	return 0;
}

MountCommand::~MountCommand()
{}

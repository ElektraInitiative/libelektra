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

#include <algorithm>
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
 * @brief Read in configuration and print warnings
 *
 * @post will update mountConf
 */
void MountCommand::readMountConf()
{
	Key parentKey(Backends::mountpointsPath, KEY_END);

	kdb::KDB kdb (parentKey);
	kdb.get(mountConf, parentKey);
	kdb.close (parentKey);

	printWarnings (cerr, parentKey);
}

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

/**
 * @brief Check for rootkey in  mountConf and add one if missing
 *
 * @param cl.verbose print text when it is missing
 */
void MountCommand::fixRootKey(Cmdline const& cl)
{
	Key rootKey (Backends::mountpointsPath, KEY_END);

	Key cur;
	cur = mountConf.lookup(rootKey);
	if (!cur)
	{
		if (cl.verbose)
		{
			cout << "Did not find the root key, will add it" << endl;
		}
		mountConf.append ( *Key(Backends::mountpointsPath,
			KEY_COMMENT, "Below are the mountpoints.",
			KEY_END));
		mountConf.rewind();
	}
}

/**
 * @brief Set variable name (either interactive or by parameter)
 */
void MountCommand::getName(Cmdline const& cl)
{
	std::vector <std::string> names;
	Backends::BackendInfoVector info = Backends::getBackendInfo(mountConf);
	names.push_back("default");
	for (Backends::BackendInfoVector::const_iterator it=info.begin();
			it!=info.end(); ++it)
	{
		names.push_back(it->name);
	}

	if (cl.interactive)
	{
		cout << "Already used are: ";
		std::copy (names.begin(), names.end(), ostream_iterator<std::string>(cout, " "));
		cout << endl;
		std::cout << "Backend name: ";
		cin >> name;
	}
	else
	{
		name = cl.arguments[1];
		if (name == "/")
		{
			name = "root";
		}
		else
		{
			std::replace(name.begin(), name.end(), '/', '_');
		}
	}

	if (std::find(names.begin(), names.end(), name) != names.end()) throw NameAlreadyInUseException();
}

/**
 * @brief set mp (interactive or by commandline)
 *
 * @pre name must be set before
 * @see getName()
 */
void MountCommand::getMountpoint(Cmdline const& cl)
{
	Key cur;
	std::vector <std::string> mountpoints;
	mountpoints.push_back("system/elektra");
	mountConf.rewind();
	while (cur = mountConf.next())
	{
		if (cur.getBaseName() == "mountpoint")
		{
			if (cur.getString().at(0) == '/')
			{
				mountpoints.push_back(Key ("user" + cur.getString(), KEY_END).getName());
				mountpoints.push_back(Key ("system" + cur.getString(), KEY_END).getName());
			} else {
				mountpoints.push_back(cur.getString());
			}
		};
	}

	mp = "/";
	if (name != "root")
	{
		if (cl.interactive)
		{
			cout << "Already used are: ";
			std::copy (mountpoints.begin(), mountpoints.end(), ostream_iterator<std::string>(cout, " "));
			cout << endl;
			cout << "Please start with / for a cascading backend" << endl;
			cout << "Enter the mountpoint: ";
			cin >> mp;
		}
		else
		{
			mp = cl.arguments[1];
		}
	}

	if (mp.at(0) == '/')
	{
		Key skmp ("system" + mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), skmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
		Key ukmp ("user" + mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), ukmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
	} else {
		Key kmp (mp, KEY_END);
		if (!kmp.isValid()) throw MountpointNotValid();
		if (std::find(mountpoints.begin(), mountpoints.end(), kmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
	}
}

void MountCommand::buildBackend(Cmdline const& cl)
{
	Backend backend (name, mp);

	if (cl.debug)
	{
		cout << "Trying to load the resolver plugin" << endl;
	}

	backend.addPlugin ("resolver");

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

	std::string configPath = Backends::mountpointsPath;
	configPath += "/";
	configPath += name;
	configPath += "/config";
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

void MountCommand::appendPlugins(Cmdline const& cl, Backend & backend)
{
	std::string pname;
	size_t current_plugin = 2;
	if (cl.interactive)
	{
		cout << "First Plugin: ";
		cin >> pname;
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
			cout << "Could not add that plugin" << endl;
			cout << e.what() << endl;
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
			}
			current_plugin ++;
		}

		if (pname == "." && !backend.validated())
		{
			throw CommandAbortException();
		}
	}
}

void MountCommand::askForConfirmation(Cmdline const& cl)
{
	if (cl.interactive)
	{
		cout << endl;
		cout << "Ready to mount with following configuration:" << endl;
		cout << "Name:       " << name << endl;
		cout << "Mountpoint: " << mp << endl;
		cout << "Path:       " << path << endl;
	}

	if (cl.debug)
	{
		cout << "The configuration which will be set is:" << endl;
		mountConf.rewind();
		while (Key k = mountConf.next())
		{
			cout << k.getName() << " " << k.getString() << endl;
		}
	}

	if (cl.interactive)
	{
		cout << "Are you sure you want to do that (y/N): ";
		std::string answer;
		cin >> answer;
		if (answer != "y") throw CommandAbortException();
	}

	if (cl.debug)
	{
		cout << "Now writing the mountpoint configuration";
	}
}

/**
 * @brief Really write out config
 */
void MountCommand::doIt()
{
	Key parentKey(Backends::mountpointsPath, KEY_END);

	kdb::KDB kdb (parentKey);
	kdb.set(mountConf, parentKey);
	kdb.close (parentKey);

	printWarnings(cerr, parentKey);
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

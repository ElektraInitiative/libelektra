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

MountCommand::MountCommand()
{}

KeySet MountCommand::readMountConf()
{
	Key parentKey("system/elektra/mountpoints", KEY_END);

	kdb::KDB kdb (parentKey);
	kdb.get(mountConf, parentKey);
	kdb.close (parentKey);

	printError(cerr, parentKey);
	printWarnings (cerr, parentKey);

	return mountConf;
}

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

	if (cl.debug)
	{
		cout << "Already used are: ";
		std::copy (names.begin(), names.end(), ostream_iterator<std::string>(cout, " "));
		cout << endl;
	}

	if (cl.interactive)
	{
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
		cout << "Already used are: ";
		std::copy (mountpoints.begin(), mountpoints.end(), ostream_iterator<std::string>(cout, " "));
		cout << endl;
		if (cl.interactive)
		{
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

	backend.tryPlugin ("resolver");

	if (cl.interactive)
	{
		cout << endl;
		cout << "Enter a path to a file in the filesystem" << endl;
		cout << "This is used by all plugins of this backend as fallback" << endl;
		cout << "It must be provided and must be a valid path" << endl;
		cout << "For user or cascading mountpoints it must be a relative path." << endl;
		cout << "The actual path will be located dynamically by the resolver plugin." << endl;
		cout << "Path: ";
		cin >> path;
	}
	else
	{
		path = cl.arguments[0];
	}

	backend.checkFile (path);

	backend.addPlugin ();

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
	cout << endl;


	if (cl.interactive)
	{
		cout << "Now enter a sequence of plugins you want in the backend" << endl;
	}

	const size_t current_plugin = 2;
	if (cl.interactive)
	{
		cout << "First Plugin: ";
		cin >> name;
	}
	else
	{
		if (current_plugin >=  cl.arguments.size())
		{
			name = "dump";
		}
		else
		{
			name = cl.arguments[current_plugin];
		}
	}

	appendPlugins(cl, backend);

	Key rootKey (Backends::mountpointsPath, KEY_END);
	backend.serialize (rootKey, mountConf);
}

void MountCommand::appendPlugins(Cmdline const& cl, Backend & backend)
{
	size_t current_plugin = 3;

	while (name != "." || !backend.validated())
	{
		try {
			backend.tryPlugin (name);
			backend.addPlugin ();
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
			cin >> name;
		}
		else
		{
			if (current_plugin >=  cl.arguments.size())
			{
				name = ".";
			}
			else
			{
				name = cl.arguments[current_plugin];
			}
			current_plugin ++;
		}

		if (name == "." && !backend.validated())
		{
			throw CommandAbortException();
		}
	}
}

void MountCommand::askForConfirmation(Cmdline const& cl)
{
	if (cl.debug)
	{
		cout << "Ready to mount with following configuration:" << endl;
		cout << "Name:       " << name << endl;
		cout << "Mountpoint: " << mp << endl;
		cout << "Path:       " << path << endl;
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

void MountCommand::doIt()
{
	Key parentKey(Backends::mountpointsPath, KEY_END);

	kdb::KDB kdb (parentKey);
	cout << ".";
	KeySet dummy;
	kdb.get(dummy, parentKey);
	cout << ".";
	kdb.set(mountConf, parentKey);
	cout << ".";
	kdb.close (parentKey);
	cout << endl;

	printError(cerr, parentKey);
	printWarnings(cerr, parentKey);
}

/**
 * @brief Its quite linear whats going on, but there are many steps involved
 *
 * @param cl the commandline
 *
 * @return 
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

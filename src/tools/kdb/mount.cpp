#include <mount.hpp>
#include <backend.hpp>
#include <cmdline.hpp>
#include <print.hpp>

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

std::string MountCommand::root = "system/elektra/mountpoints";

MountCommand::MountCommand()
{}

KeySet readMountConf()
{
	KeySet mountConf;
	Key parentKey("system/elektra/mountpoints", KEY_END);

	kdb::KDB kdb (parentKey);
	kdb.get(mountConf, parentKey);
	kdb.close (parentKey);

	printError(parentKey);
	printWarnings (parentKey);

	return mountConf;
}

void MountCommand::outputMtab()
{
	KeySet mountConf = readMountConf();

	Key rootKey (root, KEY_END);
	Key cur;

	mountConf.rewind();
	while (cur = mountConf.next())
	{
		if (rootKey.isDirectBelow(cur))
		{
			Key path = mountConf.lookup (cur.getName() + "/config/path");
			if (!path)
			{
				cout << cur.getName() << " has no path" << endl;
				continue;
			}
			Key mp = mountConf.lookup (cur.getName() + "/mountpoint");
			if (!mp)
			{
				cout << cur.getName() << " has no mountpoint" << endl;
				continue;
			}

			cout << path.getString() << " on " << mp.getString() << " with name " << cur.getBaseName() << endl;
		}
	}
}

int MountCommand::execute(Cmdline const& cl)
{
	size_t argc = cl.arguments.size();

	if (!cl.interactive && argc == 0)
	{
		// no interactive mode, so lets output the mtab
		outputMtab();
		return 0;
	}

	if (!cl.interactive && argc == 1)
	{
		throw invalid_argument("wrong number of arguments, 0 or more then 1 needed");
	}

	if (cl.debug)
	{
		cout << "Note that nothing will be written out" << endl;
		cout << "until you say y at the very end of the mounting process" << endl;
	}

	if (cl.interactive)
	{
		cout << "Welcome to interactive mounting" << endl;
		cout << "Please provide a unique name." << endl;

	}

	KeySet mountConf = readMountConf();

	Key rootKey (root, KEY_END);

	Key cur;
	cur = mountConf.lookup(rootKey);
	if (!cur)
	{
		if (cl.verbose)
		{
			cout << "Did not find the root key, will add it" << endl;
		}
		mountConf.append ( *Key(root,
			KEY_COMMENT, "Below are the mountpoints.",
			KEY_END));
		mountConf.rewind();
	}

	std::vector <std::string> names;
	names.push_back("default");
	while (cur = mountConf.next())
	{
		if (rootKey.isDirectBelow(cur))
		{
			names.push_back(cur.getBaseName());
		}
	}

	cout << "Already used are: ";
	std::copy (names.begin(), names.end(), ostream_iterator<std::string>(cout, " "));
	cout << endl;

	std::string name;
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
	cout << endl;

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

	std::string mp = "/";
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




	std::string path;
	{
		Backend backend (name, mp);

		cout << "Trying to load the resolver plugin" << endl;

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

		try
		{
			backend.checkFile (path);
		}
		catch(FileNotValidException const& e)
		{
			cout << "Invalid path " << path << ": " << e.what() << endl;
			return 1;
		}

		backend.addPlugin ();

		mountConf.append ( *Key( root  + "/" + name + "/config",
				KEY_VALUE, "",
				KEY_COMMENT, "This is a configuration for a backend, see subkeys for more information",
				KEY_END));
		mountConf.append ( *Key( root  + "/" + name + "/config/path",
				KEY_VALUE, path.c_str(),
				KEY_COMMENT, "The path for this backend. Note that plugins can override that with more specific configuration.",
				KEY_END));
		cout << endl;


		cout << "Now enter a sequence of plugins you want in the backend" << endl;

		size_t current_plugin = 2;
		if (cl.interactive)
		{
			cout << "First Plugin: ";
			cin >> name;
		}
		else
		{
			if (current_plugin >= argc)
			{
				name = "dump";
			}
			else
			{
				name = cl.arguments[current_plugin];
			}
			current_plugin ++;
		}

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
				if (current_plugin >= argc)
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

		backend.serialize (rootKey, mountConf);
	}


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
		cout << "Are you sure you want to do that (y/N): ";
		std::string answer;
		cin >> answer;
		if (answer != "y") throw CommandAbortException();
	}


	cout << "Now writing the mountpoint configuration";
	{
		Key parentKey(root, KEY_END);

		kdb::KDB kdb (parentKey);
		cout << ".";
		KeySet dummy;
		kdb.get(dummy, parentKey);
		cout << ".";
		kdb.set(mountConf, parentKey);
		cout << ".";
		kdb.close (parentKey);
		cout << endl;

		printError(parentKey);
		printWarnings(parentKey);
	}

	return 0;
}

MountCommand::~MountCommand()
{}

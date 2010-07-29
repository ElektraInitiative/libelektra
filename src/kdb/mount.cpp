#include <mount.hpp>
#include <backend.hpp>

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

bool MountCommand::checkFile(std::string path)
{
	if (path[0] == '/')
	{
		cerr << "You must use an relative path" << endl;
		return false;
	}
	return true;
}

void MountCommand::outputMtab()
{
	KeySet mountConf;

	{
		Key parentKey("system/elektra/mountpoints", KEY_END);

		kdb::KDB kdb (parentKey);
		kdb.get(mountConf, parentKey);
		kdb.close (parentKey);

		printWarnings (parentKey);

		// now we dont need any affairs to the key database
	}

	Key rootKey (root, KEY_END);
	Key cur;

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

int MountCommand::execute(int argc, char** argv)
{
	if (argc == 2)
	{
		// no arguments, just output mountpoints
		outputMtab();
		return 0;
	}

	cout << "Welcome to interactive mounting" << endl;
	cout << "Please provide a unique name." << endl;

	KeySet wholeConf;

	{
		Key parentKey("", KEY_END);

		kdb::KDB kdb (parentKey);
		kdb.get(wholeConf, parentKey);
		kdb.close (parentKey);

		printWarnings (parentKey);

		// now we dont need any affairs to the key database
	}

	Key rootKey (root, KEY_END);
	KeySet conf = wholeConf.cut (rootKey);

	Key cur;
       
	cur = conf.lookup(rootKey);
	if (!cur)
	{
		cout << "Did not find the root key, will add it" << endl;
		cout << "Note that nothing will be written out" << endl;
		cout << "until you say y at the very end of the mounting process" << endl;
		conf.append ( *Key(root,
			KEY_COMMENT, "Below are the mountpoints.",
			KEY_END));
		conf.rewind();
	}

	std::vector <std::string> names;
	names.push_back("default");
	while (cur = conf.next())
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
	std::cout << "Backend name: ";
	cin >> name;
	if (std::find(names.begin(), names.end(), name) != names.end()) throw NameAlreadyInUseException();
	cout << endl;

	std::vector <std::string> mountpoints;
	mountpoints.push_back("system/elektra");
	conf.rewind();
	while (cur = conf.next())
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
		cout << "Please start with / for a cascading backend" << endl;
		cout << "Enter the mountpoint: ";
		cin >> mp;
	}

	if (mp.at(0) == '/')
	{
		Key skmp ("system" + mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), skmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
		Key ukmp ("user" + mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), ukmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
	} else {
		Key kmp (mp, KEY_END);
		if (std::find(mountpoints.begin(), mountpoints.end(), kmp.getName()) != mountpoints.end()) throw MountpointAlreadyInUseException();
	}

	cout << endl;




	std::string path;
	{
		Backend backend (name, mp);

		cout << "Trying to load the resolver plugin" << endl;

		backend.tryPlugin ("resolver");

		cout << "Enter a path to a file in the filesystem" << endl;
		cout << "This is used by all plugins of this backend as fallback" << endl;
		cout << "It must be provided and must be a valid path" << endl;
		cout << "Path: ";
		cin >> path;
		backend.checkFile (path);
		backend.addPlugin ();

		conf.append ( *Key( root  + "/" + name + "/config",
				KEY_VALUE, "",
				KEY_COMMENT, "This is a configuration for a backend, see subkeys for more information",
				KEY_END));
		conf.append ( *Key( root  + "/" + name + "/config/path",
				KEY_VALUE, path.c_str(),
				KEY_COMMENT, "The path for this backend. Note that plugins can override that with more specific configuration.",
				KEY_END));
		cout << endl;


		cout << "Now enter a sequence of plugins you want in the backend" << endl;

		std::string name;
		cout << "First Plugin: ";
		cin >> name;
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
			if (!backend.validated()) cout << "Not validated, try to add another plugin (. to abort)" << endl;
			else cout << "Enter . to finish entering plugins" << endl;

			cout << endl;
			cout << "Next Plugin: ";
			cin >> name;

			if (name == "." && !backend.validated())
			{
				throw CommandAbortException();
			}
		}

		backend.serialize (rootKey, conf);
	}


	cout << "Ready to mount with following configuration:" << endl;
	cout << "Name:       " << name << endl;
	cout << "Mountpoint: " << mp << endl;
	cout << "Path:       " << path << endl;
	cout << "The configuration which will be set is:" << endl;
	conf.rewind();
	while (Key k = conf.next())
	{
		cout << k.getName() << " " << k.getString() << endl;
	}
	cout << "Are you sure you want to do that (y/N): ";
	std::string answer;
	cin >> answer;
	if (answer != "y") throw CommandAbortException();


	cout << "Setting the mountpoint configuration";
	{
		Key parentKey(root, KEY_END);

		kdb::KDB kdb (parentKey);
		cout << ".";
		KeySet dummy;
		kdb.get(dummy, parentKey);
		cout << ".";
		kdb.set(conf, parentKey);
		cout << ".";
		kdb.close (parentKey);
		cout << endl;

		printWarnings (parentKey);

		// now we dont need any affairs to the key database
	}

	wholeConf.append(conf);
	cout << "Writing back configuration to new mounted backends";
	{
		Key parentKey("", KEY_END);

		kdb::KDB kdb (parentKey);
		KeySet dummy;
		cout << ".";
		kdb.get(dummy, parentKey);
		cout << ".";
		kdb.set(wholeConf, parentKey);
		cout << ".";
		kdb.close (parentKey);
		cout << endl;

		printWarnings (parentKey);

		// now we dont need any affairs to the key database
	}

	return 0;
}

MountCommand::~MountCommand()
{}

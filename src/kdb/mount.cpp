#include <mount.hpp>

#include <algorithm>
#include <iostream>
#include <iterator>
#include <fstream>
#include <vector>
#include <string>

#include <kdbinternal.h>

using namespace std;
using namespace kdb;

std::string MountCommand::root = "system/elektra/mountpoints";

MountCommand::MountCommand()
{}

KeySet MountCommand::addPlugins(std::string name, std::string which)
{
	KeySet ret;
	ret.append (*Key (root + "/" + name + "/" + which + "plugins",
		KEY_COMMENT, "List of plugins to use",
		KEY_END));
	cout << "Now you have to provide some " << which << " plugins which should be used for that backend" << endl;
	for (int i=0; i<10; ++i)
	{
		cout << "Enter the " << i << " plugin to use." << endl;
		cout << "Write \".\" as name in a single line if you are finished" << endl;
		cout << "Name of the " << which << " plugin: ";
		std::string pluginName;
		cin >> pluginName;
		if (pluginName == ".") break;

		cout << "Enter a path to a file in the filesystem: ";
		std::string path;
		cin >> path;

		std::ofstream f(path.c_str());
		if (!f.is_open()) cerr << "Warning, could not open that file" << endl;
		KeySet testConfig(1,
			*Key(	"system/path",
				KEY_VALUE, path.c_str(),
				KEY_COMMENT, "Test config for loading a plugin.",
				KEY_END),
			KS_END);

		ckdb::Plugin *plugin = ckdb::pluginOpen(pluginName.c_str(), testConfig.dup());
		if (plugin && which == "get")
		{
			if (!plugin->kdbGet)
			{
				cout << "get symbol missing" << endl;
				plugin = 0;
			}
		}
		if (plugin && which == "set")
		{
			if (!plugin->kdbSet)
			{
				cout << "set symbol missing" << endl;
				plugin = 0;
			}
		}
		ckdb::pluginClose(plugin);
		if (!plugin)
		{
			cout << "Was not able to load such a plugin!" << endl;
			cout << "or it had no " << which << " symbol exported (see above)" << endl;
			cout << "Do you want to (P)roceed with next plugin?" << endl;
			cout << "Do you want to (R)etry?" << endl;
			cout << "Do you want to (F)inish entering plugins?" << endl;
			cout << "Or do you want to (A)bort?" << endl;
			string answer;
			cin >> answer;
			if (answer == "P" || answer == "Proceed" || answer == "(P)roceed" || answer == "p")
			{
				continue;
			} else if (answer == "R" || answer == "Retry" || answer == "(R)etry" || answer == "r")
			{
				--i;
				continue;
			} else if (answer == "F" || answer == "Finish" || answer == "(F)inish" || answer == "f")
			{
				break;
			} else throw 3;
		}

		std::ostringstream pluginNumber;
		pluginNumber << i;
		ret.append (*Key (root + "/" + name + "/" + which + std::string("plugins/#") + pluginNumber.str() + pluginName,
			KEY_COMMENT, "A plugin",
			KEY_END));
		ret.append (*Key (root + "/" + name + "/" + which + std::string("plugins/#") + pluginNumber.str() + pluginName + "/config",
			KEY_COMMENT, "The configuration for the specific plugin.\n"
			"All keys below that directory will be passed to plugin.\n"
			"These keys have backend specific meaning.\n"
			"See documentation http://www.libelektra.org for which keys must or can be set.\n"
			"Here the most important keys should be preloaded.",
			KEY_END));
		ret.append (*Key (root + "/" + name + "/" + which + "plugins/#" + pluginNumber.str() + pluginName + "/config/path",
			KEY_VALUE, path.c_str(),
			KEY_COMMENT, "The path where the config file is located."
			"This item is often used by backends using configuration in a filesystem"
			"to know there relative location of the keys to fetch or write.",
			KEY_END));
	}

	return ret;
}

int MountCommand::execute(int , char** )
{
	KeySet conf;
	kdb.get(conf, Key(root, KEY_END));
	cout << "Welcome to interactive mounting" << endl;
	cout << "Please provide a unique name." << endl;

	std::vector <std::string> names;
	conf.rewind();

	Key cur = conf.lookup(Key(root, KEY_END));

	if (!cur) conf.append ( *Key(root,
			KEY_COMMENT, "Below are the mountpoints.",
			KEY_END));
	else conf.rewind();

	while (cur = conf.next())
	{
		if (Key(root, KEY_END).isDirectBelow(cur))
		{
			cout << "adding " << cur.getName() << endl;
			names.push_back(cur.getBaseName());
		} else cout << "not adding " << cur.getName() << endl;
	}
	cout << "Already used are: ";
	std::copy (names.begin(), names.end(), ostream_iterator<std::string>(cout, " "));
	std::string name;
	std::cout << endl << "Name: ";
	cin >> name;
	if (std::find(names.begin(), names.end(), name) != names.end())
	{
		cerr << "Name already used, will abort" << endl;
		return 2;
	}

	conf.append ( *Key(name,
		KEY_COMMENT, "This is a mounted backend.",
		KEY_END));

	conf.append ( *Key( root  + "/" + name,
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "This is a mounted backend, see subkeys for more information",
			KEY_END));

	cout << "Please use / for the root backend" << endl;
	cout << "Enter the mountpoint: ";
	std::string mp;
	cin >> mp;
	if (mp == "/")
	{
		conf.append ( *Key(	root  + "/" + name + "/mountpoint",
				KEY_VALUE, "",
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"It must be a valid, canonical elektra path. There are no ., .. or multiple slashes allowed.\n"
				"You are not allowed to mount inside system/elektra.",
				KEY_END));
	} else {
		if (!Key (mp, KEY_END))
		{
			cerr << "This was not a valid key name" << endl;
			cerr << "Examples: system/hosts or user/sw/app" << endl;
			return 3;
		}
		conf.append ( *Key(	root  + "/" + name + "/mountpoint",
				KEY_VALUE, mp.c_str(),
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"It must be a valid, canonical elektra path. There are no ., .. or multiple slashes allowed.\n"
				"You are not allowed to mount inside system/elektra.",
				KEY_END));
	}

	conf.append(addPlugins(name, "set"));
	conf.append(addPlugins(name, "get"));

	cout << "Enter a path to a file in the filesystem (for all plugins): ";
	std::string path;
	cin >> path;

	std::ofstream f(path.c_str());
	if (!f.is_open()) cerr << "Warning, could not open that file" << endl;
	conf.append ( *Key( root  + "/" + name + "/config",
			KEY_VALUE, "",
			KEY_COMMENT, "This is a configuration for a backend, see subkeys for more information",
			KEY_END));
	conf.append ( *Key( root  + "/" + name + "/config/path",
			KEY_VALUE, path.c_str(),
			KEY_COMMENT, "The path for this backend. Note that plugins can override that with more specific configuration.",
			KEY_END));

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
	if (answer != "y")
	{
		cerr << "Aborted by user request" << endl;
		return 4;
	}

	kdb.set(conf, Key(root, KEY_END));
	return 0;
}

MountCommand::~MountCommand()
{}

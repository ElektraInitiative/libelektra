#include <mount.hpp>

#include <algorithm>
#include <iostream>
#include <iterator>
#include <fstream>
#include <vector>
#include <string>

using namespace std;
using namespace kdb;

std::string MountCommand::root = "system/elektra/mountpoints";

MountCommand::MountCommand()
{}

KeySet MountCommand::createMountPoint(std::string name, std::string mountpoint, std::string backend, std::string path)
{
	return KeySet (16,
		*Key(	root,
			KEY_DIR,
			KEY_COMMENT, "Below are the mountpoints.",
			KEY_END),
		*Key(	root  + "/" + name,
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "This is a mounted backend, see subkeys for more information",
			KEY_END),
		*Key(	root  + "/" + name + "/mountpoint",
			KEY_VALUE, mountpoint.c_str(),
			KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
			"It must be a valid, canonical elektra path. There are no ., .. or multiple slashes allowed.\n"
			"You are not allowed to mount inside system/elektra.",
			KEY_END),
		*Key(	root + "/" + name + "/backend",
			KEY_VALUE, backend.c_str(),
			KEY_COMMENT, "The name of the backend library.\n"
			"This name describes which .so should be loaded for that backend.\n"
			"You are allowed to mount the same backend multiple times.",
			KEY_END),
		*Key(	root  + "/" + name + "/config",
			KEY_VALUE, "",
			KEY_COMMENT, "The configuration for the specific backend.\n"
			"All keys below that directory will be passed to backend.\n"
			"These keys have backend specific meaning.\n"
			"See documentation http://www.libelektra.org for which keys must or can be set.\n"
			"Here the most important keys should be preloaded.",
			KEY_END),
		*Key(	root  + "/" + name + "/config/path",
			KEY_VALUE, path.c_str(),
			KEY_COMMENT, "The path where the config file is located."
			"This item is often used by backends using configuration in a filesystem"
			"to know there relative location of the keys to fetch or write.",
			KEY_END),
		KS_END);
}

int MountCommand::execute(int , char** )
{
	KeySet conf;
	kdb.get(conf, Key(root, KEY_END));
	cout << "Welcome to interactive mounting" << endl;
	cout << "Please provide a unique name." << endl;

	std::vector <std::string> names;
	conf.rewind();
	Key cur;
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
	std::cout << endl << "Please provide a name: ";
	cin >> name;
	if (std::find(names.begin(), names.end(), name) != names.end())
	{
		cerr << "Name already used, will abort" << endl;
		return 2;
	}

	cout << "Enter the mountpoint: ";
	std::string mp;
	cin >> mp;
	if (!Key (mp, KEY_END))
	{
		cerr << "This was not a valid key name" << endl;
		cerr << "Examples: system/hosts or user/sw/app" << endl;
		return 3;
	}

	cout << "Enter which plugin to use: ";
	std::string plugin;
	cin >> plugin;

	cout << "Enter a path to a file in the filesystem: ";
	std::string path;
	cin >> path;

	std::ifstream fin(path.c_str());
	if (!fin.is_open()) cerr << "Warning, could not open that file" << endl;

	cout << "Ready to mount with following configuration:" << endl;
	cout << "Name:       " << name << endl;
	cout << "Mountpoint: " << mp << endl;
	cout << "Plugin:     " << plugin << endl;
	cout << "Path:       " << path << endl;
	cout << "Are you sure you want to do that (y/N): ";
	std::string answer;
	cin >> answer;
	if (answer != "y")
	{
		cerr << "Aborted by user request" << endl;
		return 4;
	}

	conf.append(createMountPoint(name, mp, plugin, path));
	kdb.set(conf, Key(root, KEY_END));
	return 0;
}

MountCommand::~MountCommand()
{}

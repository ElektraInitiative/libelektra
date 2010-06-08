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

struct Plugin
{
	ckdb::Plugin *plugin;
	std::string pluginName;
	KeySet info;

	Plugin(std::string const& pluginName, KeySet &modules, KeySet const& testConfig) :
		pluginName(pluginName)
	{
		plugin = ckdb::elektraPluginOpen(pluginName.c_str(), modules.getKeySet(), testConfig.dup());

		if (!plugin) throw NoPlugin();

		Key infoKey ("system/elektra/modules", KEY_END);
		infoKey.addBaseName(pluginName);

		if (!plugin->kdbGet)
		{
			throw MissingSymbol("kdbGet");
		}
		plugin->kdbGet(plugin, info.getKeySet(), *infoKey);
	}

	~Plugin()
	{
		ckdb::elektraPluginClose(plugin);
	}

	ckdb::Plugin *operator->()
	{
		return plugin;
	}

	bool operator!()
	{
		return !plugin;
	}

	std::string lookupInfo(std::string item, std::string section = "infos")
	{
		Key k ("system/elektra/modules", KEY_END);
		k.addBaseName(pluginName);
		k.addBaseName(section);
		k.addBaseName(item);
		Key ret = info.lookup(k);

		if (!ret) return ""; /* Lets say missing info is ok for now */

		return ret.getString();
	}

};

MountCommand::MountCommand()
{}

bool MountCommand::checkFile(std::string path)
{
	if (path[0] != '/')
	{
		cerr << "You must use an absolute path" << endl;
		return false;
	}
	std::ofstream f(path.c_str());
	return f.is_open();
}

KeySet MountCommand::addPlugins(std::string name, KeySet& modules, KeySet& referencePlugins, std::string which)
{
	KeySet ret;
	vector <string> alreadyProvided;
	int nrStoragePlugins = 0;
	ret.append (*Key (root + "/" + name + "/" + which + "plugins",
		KEY_COMMENT, "List of plugins to use",
		KEY_END));

	cout << "Now you have to provide some " << which << " plugins which should be used for that backend" << endl;
	cout << "Exactly one plugin must be a storage plugin" << endl;
	for (int i=0; i<10; ++i)
	{
		cout << "Enter the " << i << " plugin to use." << endl;
		cout << "Write \".\" as name in a single line if you are finished" << endl;
		cout << "Name of the " << which << " plugin: ";
		std::string pluginName;
		cin >> pluginName;
		if (pluginName == ".") break;

		bool isStoragePlugin = false;

		try {
			int nr;
			char *cPluginName = 0;
			char *cReferenceName = 0;
			Key k(std::string("system/elektra/key/#0") + pluginName);
			if (ckdb::elektraProcessPlugin (*k, &nr, &cPluginName, &cReferenceName) == -1) throw BadPluginName();

			std::string realPluginName;
			if (cPluginName)
			{
				realPluginName = cPluginName;
				cout << "# seems like there is a pluginName: " << realPluginName << endl;

				if (cReferenceName)
				{
					std::string referenceName = cReferenceName;
					cout << "# and a reference name too: " << referenceName << endl;
					if (referenceName.find('#') != string::npos) throw BadPluginName();

					referencePlugins.append(Key(cReferenceName, KEY_VALUE, cPluginName, KEY_END));
				}
			} else {
				cout << "# backreference: " << cReferenceName << endl;
				Key lookup = referencePlugins.lookup(cReferenceName);
				if (!lookup) throw ReferenceNotFound();
				realPluginName = lookup.getString();
			}

			if (realPluginName.find('#') != string::npos) throw BadPluginName();


			KeySet testConfig(1,
				*Key(	"system/test",
					KEY_VALUE, "test",
					KEY_COMMENT, "Test config for loading a plugin.",
					KEY_END),
				KS_END);
			Plugin plugin (realPluginName, modules, testConfig);

			std::string provide;
			std::stringstream ss(plugin.lookupInfo("provides"));
			while (ss >> provide)
			{
				alreadyProvided.push_back(provide);
				cout << "add provide: " << provide << endl;
			}
			std::string need;
			std::stringstream nss(plugin.lookupInfo("needs"));
			while (nss >> need)
			{
				cout << "check for need " << need << endl;;
				if (std::find(alreadyProvided.begin(), alreadyProvided.end(), need) == alreadyProvided.end())
				{
					throw MissingNeeded(need);
				}
			}
			if (std::string(plugin.lookupInfo("provides")).find("storage") != string::npos)
			{
				cout << "This is a storage plugin" << endl;
				++ nrStoragePlugins;
				isStoragePlugin = true;
			}
			if (std::string(plugin.lookupInfo("licence")).find("BSD") == string::npos)
			{
				cout << "Warning this plugin is not BSD licenced" << endl;
				cout << "It might taint the licence of the overall product" << endl;
				cout << "Its licence is: " << plugin->licence << endl;
			}
			if (which == "set")
			{
				if (!plugin->kdbSet)
				{
					throw MissingSymbol("kdbSet");
				}
			}
			if (nrStoragePlugins>1)
			{
				throw StoragePlugin();
			}
		}
		catch (PluginCheckException const& e)
		{
			cout << "There is a problem with this plugin" << endl;
			cout << e.what() << endl;
			cout << "Do you want to (P)roceed with next plugin (current will be left empty)?" << endl;
			cout << "Do you want to go (B)ack to the first plugin?" << endl;
			cout << "Do you want to (R)etry this plugin?" << endl;
			cout << "Do you want to (F)inish entering plugins?" << endl;
			cout << "Or do you want to (A)bort?" << endl;
			cout << "Please provide action: ";
			string answer;
			cin >> answer;
			if (answer == "P" || answer == "Proceed" || answer == "(P)roceed" || answer == "p")
			{
				if (isStoragePlugin) --nrStoragePlugins;
				continue;
			} else if (answer == "R" || answer == "Retry" || answer == "(R)etry" || answer == "r")
			{
				if (isStoragePlugin) --nrStoragePlugins;
				--i;
				continue;
			} else if (answer == "B" || answer == "Back" || answer == "(B)ack" || answer == "b")
			{
				cout << endl;
				return KeySet(static_cast<ckdb::KeySet*>(0));
			} else if (answer == "F" || answer == "Finish" || answer == "(F)inish" || answer == "f")
			{
				if (isStoragePlugin) --nrStoragePlugins;
				break;
			}
			throw CommandAbortException();
		}

		std::ostringstream pluginNumber;
		pluginNumber << i;
		ret.append (*Key (root + "/" + name + "/" + which + std::string("plugins/#") + pluginNumber.str() + pluginName,
			KEY_COMMENT, "A plugin",
			KEY_END));
	}

	if (nrStoragePlugins != 1)
	{
		cerr << "You need to provide a storage plugin, but did not" << endl;
		cerr << "Will go back to the first plugin" << endl;
		cout << endl;
		return KeySet(static_cast<ckdb::KeySet*>(0));
	}

	cout << endl;
	return ret;
}

int MountCommand::execute(int , char** )
{
	cout << "Welcome to interactive mounting" << endl;
	cout << "Please provide a unique name." << endl;

	KeySet conf;
	try {
		kdb.get(conf, Key(root, KEY_END));
	} catch (KDBException const& e)
	{
		cout << "Could not get configuration" << endl;
		cout << "Seems like this is your first mount" << endl;
	}

	conf.rewind();

	Key cur;
       
	cur = conf.lookup(Key(root, KEY_END));
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
	while (cur = conf.next())
	{
		if (Key(root, KEY_END).isDirectBelow(cur))
		{
			names.push_back(cur.getBaseName());
		}
	}
	cout << "Already used are: ";
	std::copy (names.begin(), names.end(), ostream_iterator<std::string>(cout, " "));
	cout << endl;
	std::string name;
	std::cout << "Name: ";
	cin >> name;
	if (std::find(names.begin(), names.end(), name) != names.end()) throw NameAlreadyInUseException();
	cout << endl;



	conf.append ( *Key( root  + "/" + name,
			KEY_DIR,
			KEY_VALUE, "",
			KEY_COMMENT, "This is a mounted backend, see subkeys for more information",
			KEY_END));

	std::vector <std::string> mountpoints;
	KeySet ksMountpoints;
	conf.rewind();
	while (cur = conf.next())
	{
		if (cur.getBaseName() == "mountpoint")
		{
			if (cur.getString() == "")
			{
				mountpoints.push_back("/");
			} else {
				if (!conf.lookup(Key(cur.getString(), KEY_END)))
				{
					cout << "Did not find the mountpoint " << cur.getString() << ", will add it" << endl;
					// Hack: currently the mountpoints need to exist, so that they can be found
					ksMountpoints.append ( *Key(cur.getString(),
						KEY_COMMENT, "This is a mountpoint",
						KEY_META, "mountpoint", "",
						KEY_END));
				}
				mountpoints.push_back(cur.getString());
			}
		};
	}

	conf.append(ksMountpoints);
	cout << "Already used are: ";
	std::copy (mountpoints.begin(), mountpoints.end(), ostream_iterator<std::string>(cout, " "));
	cout << endl;
	cout << "Please use / for the root backend" << endl;
	cout << "Enter the mountpoint: ";
	std::string mp;
	cin >> mp;
	if (std::find(mountpoints.begin(), mountpoints.end(), mp) != mountpoints.end()) throw MountpointAlreadyInUseException();

	if (mp == "/")
	{
		conf.append ( *Key(	root  + "/" + name + "/mountpoint",
				KEY_VALUE, "",
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"It must be a valid, canonical elektra path. There are no ., .. or multiple slashes allowed.\n"
				"You are not allowed to mount inside system/elektra.",
				KEY_END));
	} else {
		if (!Key (mp, KEY_END)) throw MountpointInvalidException();
		// Hack: currently the mountpoints need to exist, so that they can be found
		conf.append ( *Key(mp,
			KEY_COMMENT, "This is a mounted backend.",
			KEY_META, "mountpoint", "",
			KEY_END));
		conf.append ( *Key(	root  + "/" + name + "/mountpoint",
				KEY_VALUE, mp.c_str(),
				KEY_COMMENT, "The mountpoint says the location where the backend should be mounted.\n"
				"It must be a valid, canonical elektra path. There are no ., .. or multiple slashes allowed.\n"
				"You are not allowed to mount inside system/elektra.",
				KEY_END));
	}
	cout << endl;



	cout << "Enter a path to a file in the filesystem" << endl;
	cout << "This is used by all plugins of this backend as fallback" << endl;
	cout << "It must be provided and must be a valid path" << endl;
	cout << "Path: ";
	std::string path;
	cin >> path;
	if (!checkFile(path)) throw PathInvalidException();
	conf.append ( *Key( root  + "/" + name + "/config",
			KEY_VALUE, "",
			KEY_COMMENT, "This is a configuration for a backend, see subkeys for more information",
			KEY_END));
	conf.append ( *Key( root  + "/" + name + "/config/path",
			KEY_VALUE, path.c_str(),
			KEY_COMMENT, "The path for this backend. Note that plugins can override that with more specific configuration.",
			KEY_END));
	cout << endl;




	KeySet modules;
	KeySet referencePlugins;
	elektraModulesInit(modules.getKeySet(), 0);
	while (conf.append(addPlugins(name, modules, referencePlugins, "get")) == -1) ;



	while (conf.append(addPlugins(name, modules, referencePlugins, "set")) == -1) ;
	elektraModulesClose(modules.getKeySet(), 0);



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

	kdb.set(conf, Key());
	return 0;
}

MountCommand::~MountCommand()
{}

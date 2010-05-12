#include <mount.hpp>

#include <iostream>
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

int MountCommand::execute(int argc, char** argv)
{
	if (argc == 6)
	{
		KeySet conf;
		kdb.get(conf, Key(root, KEY_END));
		conf.append(createMountPoint(argv[2], argv[3], argv[4], argv[5]));
		kdb.set(conf, Key(root, KEY_END));
	} else {
		cout << "mount <name> <root> <backend> <path>" << endl;
		return 1;
	}
	return 0;
}

MountCommand::~MountCommand()
{}

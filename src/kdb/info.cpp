#include <info.hpp>

#include <kdb>

#include <iostream>

#include <kdbmodule.h>

#include <plugin.hpp>

using namespace std;
using namespace kdb;

InfoCommand::InfoCommand()
{}

int InfoCommand::execute(int argc, char** argv)
{
	if (argc != 3)
	{
		cerr << "Please provide a module name" << endl;
		cerr << "Usage: get <name>" << endl;
		return 1;
	}

	std::string name = argv[2];

	KeySet conf;
	Key parentKey(std::string("system/elektra/modules/") + name, KEY_END);

	kdb.get(conf, parentKey);

	if (!conf.lookup(parentKey))
	{
		cerr << "Seems like module configuration is broken." << endl;
		cerr << "This presents a severe problem for most application." << endl;
		cerr << "Maybe the mountpoint configuration is broken." << endl;
		cerr << "Now in fallback code. Will directly load config from plugin" << endl;
		KeySet modules;
		elektraModulesInit(modules.getKeySet(), 0);
		KeySet testConfig(1,
			*Key(	"system/test",
				KEY_VALUE, "test",
				KEY_COMMENT, "Test config for loading a plugin.",
				KEY_END),
			KS_END);
		Plugin plugin (name, modules, testConfig);
		plugin.loadInfo();
		elektraModulesClose(modules.getKeySet(), 0);
		// TODO: memory leak
		conf.append(plugin.getInfo());
	}

	Key root (std::string("system/elektra/modules/") + name + "/exports", KEY_END);
	Key k = conf.lookup (root);

	if (k)
	{
		cout << "Exported symbols: ";
		while ((k = conf.next()) && k.getDirName() == root.getName())
		{
			cout << k.baseName() << " ";
		}
		cout << endl;
	}
	else cout << "no exported symbols found" << endl;

	root.setName(std::string("system/elektra/modules/") + name + "/infos");
	k = conf.lookup (root);

	if (k)
	{
		while ((k = conf.next()) && k.getDirName() == root.getName())
		{
			cout << k.baseName() << ": " << k.getString() << endl;
		}
	} else cout << "no information found" << endl;

	return 0;
}

InfoCommand::~InfoCommand()
{}

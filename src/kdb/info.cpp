#include <info.hpp>

#include <kdb>

#include <iostream>

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
	kdb.get(conf, Key(std::string("system/elektra/modules/") + name, KEY_END));


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

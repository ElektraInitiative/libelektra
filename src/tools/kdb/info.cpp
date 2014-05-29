#include <info.hpp>

#include <kdb.hpp>
#include <plugin.hpp>
#include <modules.hpp>
#include <cmdline.hpp>

#include <iostream>


using namespace std;
using namespace kdb;

InfoCommand::InfoCommand()
{}

int InfoCommand::execute(Cmdline const& cl)
{
	std::string subkey;
	if (cl.arguments.size() == 1)
	{
	}
	else if (cl.arguments.size() == 2)
	{
		subkey = cl.arguments[1];
	}
	else
	{
		throw invalid_argument("Need 1 or 2 argument(s)");
	}
	std::string name = cl.arguments[0];

	KeySet conf;
	Key parentKey(std::string("system/elektra/modules/") + name, KEY_END);

	if (!cl.load)
	{

		kdb.get(conf, parentKey);
	}

	if (!conf.lookup(parentKey))
	{
		if (!cl.load)
		{
			cerr << "Module does not seem to be loaded." << endl;
			cerr << "Now in fallback code. Will directly load config from plugin." << endl;
		}

		Modules modules;
		PluginPtr plugin = modules.load(name);
		conf.append(plugin->getInfo());
	}

	Key root (std::string("system/elektra/modules/") + name + "/exports", KEY_END);

	if (!subkey.empty())
	{
		root.setName(std::string("system/elektra/modules/") + name + "/infos/" + subkey);
		Key k = conf.lookup (root);
		if (k)
		{
			cout << k.getString() << std::endl;
			return 0;
		}
		else
		{
			cerr << "clause not found" << std::endl;
			return 1;
		}
	}

	root.setName(std::string("system/elektra/modules/") + name + "/exports");
	Key k = conf.lookup (root);

	if (k)
	{
		cout << "Exported symbols: ";
		while ((k = conf.next()) && k.getDirName() == root.getName())
		{
			cout << k.getBaseName() << " ";
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
			cout << k.getBaseName() << ": " << k.getString() << endl;
		}
	} else cout << "no information found" << endl;

	return 0;
}

InfoCommand::~InfoCommand()
{}

#include <info.hpp>

#include <kdb.hpp>
#include <plugin.hpp>
#include <modules.hpp>

#include <iostream>


using namespace std;
using namespace kdb;

InfoCommand::InfoCommand()
{}

int InfoCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument("Need 1 argument");
	std::string name = cl.arguments[0];

	KeySet conf;
	Key parentKey(std::string("system/elektra/modules/") + name, KEY_END);

	kdb.get(conf, parentKey);

	if (!conf.lookup(parentKey))
	{
		cerr << "Module does not seem to be loaded." << endl;
		cerr << "Now in fallback code. Will directly load config from plugin." << endl;
		// TODO: use plugin_loader here!
		Modules modules;
		std::auto_ptr<Plugin> plugin = modules.load(name);
		// TODO: memory leak??
		conf.append(plugin->getInfo());
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

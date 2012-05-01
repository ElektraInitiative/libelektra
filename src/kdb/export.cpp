#include <export.hpp>

#include <kdb.hpp>
#include <modules.hpp>

#include <iostream>
#include <memory>

using namespace kdb;
using namespace std;

ExportCommand::ExportCommand()
{}

int ExportCommand::execute(int argc, char** argv)
{
	if (argc != 4)
	{
		cerr << "Please provide a name" << endl;
		cerr << "Usage: export <format> <name>" << endl;
		return 1;
	}

	std::string format (argv[2]);

	Key root (argv[3], KEY_END);
	if (!root.isValid())
	{
		cerr << "Not a valid name supplied" << endl;
		return 1;
	}

	kdb.get(ks, root);
	printWarnings(root);

	KeySet part (ks.cut(root));

	Modules modules;
	auto_ptr<Plugin> plugin = modules.load(format);

	Plugin::func_t fun = plugin->getSymbol ("serialize");
	Plugin::serialize_t ser_fun = reinterpret_cast<Plugin::serialize_t> (fun);

	ser_fun(cout, 0, part.getKeySet());

	return 0;
}

ExportCommand::~ExportCommand()
{}

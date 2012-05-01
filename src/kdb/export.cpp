#include <export.hpp>

#include <kdb.hpp>
#include <modules.hpp>

#include <iostream>
#include <memory>

using namespace kdb;
using namespace std;

ExportCommand::ExportCommand()
{}

int ExportCommand::execute(Cmdline const& cl)
{
	size_t argc = cl.arguments.size();
	if (argc != 1 && argc != 2)
	{
		throw invalid_argument("need 1 or 2 arguments");
	}

	Key root (cl.arguments[0], KEY_END);
	if (!root.isValid())
	{
		throw invalid_argument ("root key is not a valid key name");
	}

	kdb.get(ks, root);
	printWarnings(root);

	KeySet part (ks.cut(root));

	string format = "dump";
	if (argc == 2) format = cl.arguments[1];

	Modules modules;
	auto_ptr<Plugin> plugin = modules.load(format);

	Plugin::func_t fun = plugin->getSymbol ("serialize");
	Plugin::serialize_t ser_fun = reinterpret_cast<Plugin::serialize_t> (fun);

	ser_fun(cout, 0, part.getKeySet());

	return 0;
}

ExportCommand::~ExportCommand()
{}

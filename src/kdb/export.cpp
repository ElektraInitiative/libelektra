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
	if (cl.arguments.size() != 1) return -1;

	Key root (cl.arguments[0], KEY_END);
	if (!root.isValid())
	{
		cerr << cl.arguments[0] << " is not a valid root name" << endl;
		return -1;
	}

	kdb.get(ks, root);
	printWarnings(root);

	KeySet part (ks.cut(root));

	Modules modules;
	auto_ptr<Plugin> plugin = modules.load(cl.format);

	Plugin::func_t fun = plugin->getSymbol ("serialize");
	Plugin::serialize_t ser_fun = reinterpret_cast<Plugin::serialize_t> (fun);

	ser_fun(cout, 0, part.getKeySet());

	return 0;
}

ExportCommand::~ExportCommand()
{}

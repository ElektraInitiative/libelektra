#include <export.hpp>

#include <kdb.hpp>
#include <modules.hpp>
#include <cmdline.hpp>
#include <print.hpp>

#include <iostream>
#include <memory>

using namespace kdb;
using namespace std;

ExportCommand::ExportCommand()
{}

int ExportCommand::execute(Cmdline const& cl)
{
	size_t argc = cl.arguments.size();
	if (argc != 1 && argc != 2 && argc != 3)
	{
		throw invalid_argument("need 1 to 3 arguments");
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
	if (argc > 1) format = cl.arguments[1];

	string file = "/dev/stdout";
	if (argc > 2 && cl.arguments[2] != "-") file = cl.arguments[2];

	Modules modules;
	auto_ptr<Plugin> plugin = modules.load(format);

	Key errorKey;
	errorKey.setString(file);

	plugin->set(part, errorKey);

	printError(errorKey);
	printWarnings(errorKey);

	return 0;
}

ExportCommand::~ExportCommand()
{}

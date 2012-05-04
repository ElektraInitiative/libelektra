#include <import.hpp>

#include <kdb.hpp>
#include <modules.hpp>

#include <iostream>
#include <memory>

using namespace kdb;
using namespace std;

ImportCommand::ImportCommand()
{}

int ImportCommand::execute(Cmdline const& cl)
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

	KeySet importedKeys;

	string format = "dump";
	if (argc == 2) format = cl.arguments[1];

	Modules modules;
	auto_ptr<Plugin> plugin = modules.load(format);
	plugin->unserialize(importedKeys);

	if (cl.strategy == "cut")
	{
		KeySet part(ks.cut(root));
	}
	else if (cl.strategy == "overwrite")
	{
		ks.append(importedKeys);
	}
	else // default strategy preserve
	{
		importedKeys.append(ks);
	}

	kdb.set(importedKeys, root);

	return 0;
}

ImportCommand::~ImportCommand()
{}

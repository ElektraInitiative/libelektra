#include <import.hpp>

#include <kdb.hpp>
#include <modules.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>
#include <toolexception.hpp>

#include <iostream>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

ImportCommand::ImportCommand()
{}

int ImportCommand::execute(Cmdline const& cl)
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

	KeySet originalKeys;
	kdb.get(originalKeys, root);
	printWarnings(cerr, root);

	KeySet importedKeys;

	string format = "dump";
	if (argc > 1) format = cl.arguments[1];

	string file = "/dev/stdin";
	if (argc > 2 && cl.arguments[2] != "-") file = cl.arguments[2];

	Modules modules;
	PluginPtr plugin = modules.load(format);

	Key errorKey;
	errorKey.setString(file);

	plugin->get(importedKeys, errorKey);

	printWarnings(cerr, errorKey);
	printError(cerr, errorKey);

	KeySet mergedKeys;
	if (cl.strategy == "cut")
	{
		KeySet rootKeys(originalKeys.cut(root));
		mergedKeys.append(importedKeys);
		mergedKeys.append(originalKeys);
		// rootKeys are dropped
	}
	else if (cl.strategy == "overwrite")
	{
		mergedKeys.append(originalKeys);
		mergedKeys.append(importedKeys);
	}
	else
	{
		// default strategy preserve
		mergedKeys.append(importedKeys);
		mergedKeys.append(originalKeys);
	}


	if (cl.verbose)
	{
		cout << "The merged keyset with strategy " << cl.strategy << " is:" << endl;
		cout << mergedKeys;
	}

	kdb.set(mergedKeys, root);

	return 0;
}

ImportCommand::~ImportCommand()
{}

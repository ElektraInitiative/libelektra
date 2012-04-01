#include <check.hpp>

#include <kdb.hpp>

#include <iostream>

#include <kdbmodule.h>

#include <plugin.hpp>

#include <cmdline.hpp>

using namespace std;
using namespace kdb;

CheckCommand::CheckCommand()
{}

int CheckCommand::execute(int argc, char** argv)
{
	Cmdline cl (argc, argv);

	if (cl.utilName != "check")
	{
		cerr << "This is not the correct utility" << endl;
		return 3;
	}

	if (cl.invalidOpt)
	{
		cerr << "Invalid option given" << endl;
		return 1;
	}

	if (cl.V)
	{
		cerr << cl.progName << " " << cl.utilName << " version 0.1" << endl;
		return 0;
	}

	if (cl.avail() != 1 || cl.H)
	{
		cerr << "Please provide a module name" << endl;
		cerr << "Usage: check <name>" << endl;
		if (cl.H) return 0;
		return 2;
	}

	std::string name = argv[cl.param()];

	KeySet modules;
	Key errorKey;
	elektraModulesInit(modules.getKeySet(), *errorKey);

	if (cl.t)
	{
		if (cl.v) cout << "will try to load " << name << endl;
		ckdb::elektraPluginFactory ptr = elektraModulesLoad (modules.getKeySet(),
				name.c_str(), *errorKey);
		if (!ptr)
		{
			cerr << "Could not load the library" << endl;
			printError(errorKey);
			return 4;
		}
		elektraModulesClose(modules.getKeySet(), *errorKey);
		printWarnings(errorKey);
		return 0;
	}

	if (cl.v) cout << "will try check the plugin " << name << endl;

	KeySet testConfig(1,
		*Key(	"system/test",
			KEY_VALUE, "test",
			KEY_COMMENT, "Test config for loading a plugin.",
			KEY_END),
		KS_END);
	vector<string> warnings;
	try {
		Plugin plugin (name, modules, testConfig);
		plugin.loadInfo();
		plugin.parse();
		plugin.check(warnings);
	} catch (PluginCheckException const& p) {
		cerr << "Plugin did not pass all checks!" << endl;
		cerr << "See description below:" << endl;
		cerr << p.what() << endl;
	}
	if (warnings.size() > 0)
	{
		cerr << "There are " << warnings.size() << " Warnings for this plugin" << endl;
		cerr << "For high quality plugins there should be no warning" << endl;

		for (size_t i = 0; i < warnings.size(); ++i)
		{
			cerr << "Warning #" << i << ": " << warnings[i] << endl;
		}
	}
	elektraModulesClose(modules.getKeySet(), *errorKey);
	printWarnings(errorKey);

	return 0;
}

CheckCommand::~CheckCommand()
{}

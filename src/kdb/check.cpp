#include <check.hpp>

#include <kdb>

#include <iostream>

#include <kdbmodule.h>

#include <plugin.hpp>

#include <cmdline.hpp>

#include <dlfcn.h>

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

	if (cl.v)
	{
		cerr << cl.progName << " " << cl.utilName << " version 0.1" << endl;
		return 0;
	}

	if (cl.t)
	{
		cout << "try to dlopen and dlclose libelektra-dump.so" << endl;
		void *dl = dlopen ("libelektra-dump.so", RTLD_LAZY);
		if (!dl)
		{
			cerr << "Could not load the library" << endl;
			cerr << dlerror() << endl;
			return 4;
		}
		dlclose (dl);
		return 0;
	}

	if (cl.avail() != 1 || cl.h)
	{
		cerr << "Please provide a module name" << endl;
		cerr << "Usage: check <name>" << endl;
		if (cl.h) return 0;
		return 2;
	}

	std::string name = argv[cl.param()];

	cout << name << endl;

	KeySet modules;
	elektraModulesInit(modules.getKeySet(), 0);
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
	elektraModulesClose(modules.getKeySet(), 0);

	return 0;
}

CheckCommand::~CheckCommand()
{}

#include <check.hpp>

#include <kdb>

#include <iostream>

#include <kdbmodule.h>

#include <plugin.hpp>

using namespace std;
using namespace kdb;

CheckCommand::CheckCommand()
{}

int CheckCommand::execute(int argc, char** argv)
{
	if (argc != 3)
	{
		cerr << "Please provide a module name" << endl;
		cerr << "Usage: get <name>" << endl;
		return 1;
	}

	std::string name = argv[2];

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

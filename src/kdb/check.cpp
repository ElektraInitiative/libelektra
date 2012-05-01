#include <check.hpp>

#include <kdb.hpp>
#include <plugin.hpp>
#include <modules.hpp>
#include <cmdline.hpp>

#include <iostream>


using namespace std;
using namespace kdb;

CheckCommand::CheckCommand()
{}

int CheckCommand::execute(int argc, char** argv)
{
	Cmdline cl (argc, argv,
			1,
			"HvV",
			"<name>\n"
			"Do some basic checks on a plugin.\n");

	if (cl.invalidOpt)
	{
		cerr << cl << endl;
		return 1;
	}

	if (cl.V)
	{
		cerr << cl.progName << " " << cl.utilName << " version 0.1" << endl;
		return 0;
	}

	if (cl.avail() != 1 || cl.H)
	{
		if (!cl.H) cerr << "Please provide a module name\n"
			<< endl;
		cerr << cl << endl;
		if (cl.H) return 0;
		return 2;
	}

	std::string name = argv[cl.param()];

	Modules modules;
	if (cl.v) cout << "will try check the plugin " << name << endl;

	vector<string> warnings;
	try {
		std::auto_ptr<Plugin> plugin = modules.load (name);
		plugin->check(warnings);
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

	return 0;
}

CheckCommand::~CheckCommand()
{}

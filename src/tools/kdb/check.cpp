#include <check.hpp>

#include <kdb.hpp>
#include <plugin.hpp>
#include <modules.hpp>
#include <cmdline.hpp>
#include <print.hpp>

#include <iostream>


using namespace std;
using namespace kdb;

CheckCommand::CheckCommand()
{}

int CheckCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() != 1)
	{
		throw invalid_argument ("One argument required");
	}

	std::string name = cl.arguments[0];

	Modules modules;
	if (cl.verbose) cout << "will try check the plugin " << name << endl;

	vector<string> warnings;
	try {
		PluginPtr plugin = modules.load (name);
		plugin->check(warnings);

	}
	catch (NoPlugin const& p)
	{
		cerr << p.what() << endl;
		return 2;
	}
	catch (PluginCheckException const& p)
	{
		cerr << "Plugin did not pass all checks:" << endl;
		cerr << "See description below:" << endl;
		cerr << p.what() << endl;
		return 3;
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

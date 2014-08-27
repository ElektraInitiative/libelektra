#include <check.hpp>

#include <kdb.hpp>
#include <plugin.hpp>
#include <modules.hpp>
#include <cmdline.hpp>

#include <iostream>


using namespace std;
using namespace kdb;
using namespace kdb::tools;

CheckCommand::CheckCommand()
{}

int CheckCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() == 0)
	{
		Key x;
		KDB kdb (x);
		bool wo = x.getMeta<const kdb::Key>("warnings");
		bool eo = x.getMeta<const kdb::Key>("error");
		if (wo || eo) std::cerr << "opening of kdb yield following problems:" << std::endl;
		printWarnings(cerr, x);
		printError(cerr, x);

		Key y;
		kdb.close(y);
		bool wc = y.getMeta<const kdb::Key>("warnings");
		bool ec = y.getMeta<const kdb::Key>("error");
		if (wc || ec) std::cerr << "closing of kdb yield following problems:" << std::endl;
		printWarnings(cerr, y);
		printError(cerr, y);
		return  wo + wc*2 + eo * 4 + ec *8;
	}

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
		return 1;
	}
	catch (PluginCheckException const& p)
	{
		cerr << "Plugin did not pass all checks:" << endl;
		cerr << "See description below:" << endl;
		cerr << p.what() << endl;
		return 2;
	}

	if (warnings.size() > 0)
	{
		cerr << "There are " << warnings.size() << " Warnings for this plugin" << endl;
		cerr << "For high quality plugins there should be no warning" << endl;

		for (size_t i = 0; i < warnings.size(); ++i)
		{
			cerr << "Warning #" << i << ": " << warnings[i] << endl;
		}
		return 3;
	}

	return 0;
}

CheckCommand::~CheckCommand()
{}

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

int printProblems(Key k, std::string action, int off)
{
	bool wo = k.getMeta<const kdb::Key>("warnings");
	bool eo = k.getMeta<const kdb::Key>("error");
	if (wo || eo) std::cerr << action << " of kdb yield following problems:" << std::endl;
	printWarnings(cerr, k);
	return (wo + eo * 2) << off;
}

int CheckCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() == 0)
	{
		int ret = 0;
		Key x;
		KDB kdb (x);
		ret += printProblems(x, "opening", 0);

		KeySet ks;
		Key a("/", KEY_END);
		kdb.get(ks, a);
		ret += printProblems(a, "getting", 2);

		Key b("/", KEY_END);
		kdb.set(ks, b);
		ret += printProblems(b, "setting", 4);

		Key y;
		kdb.close(y);
		ret += printProblems(y, "closing", 6);
		return  ret;
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
		KeySet ks = cl.getPluginsConfig();
		PluginPtr plugin;
		if (ks.size() == 0)
		{
			plugin = modules.load(name);
		}
		else
		{
			plugin = modules.load(name, ks);
		}
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

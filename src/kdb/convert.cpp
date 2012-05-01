#include <convert.hpp>

#include <kdb.hpp>
#include <modules.hpp>

#include <iostream>
#include <memory>

using namespace kdb;
using namespace std;

ConvertCommand::ConvertCommand()
{}

int ConvertCommand::execute(Cmdline const& cl)
{
	size_t argc = cl.arguments.size();
	if (argc != 0 && argc != 1 && argc != 2)
	{
		throw invalid_argument("need 0, 1 or 2 arguments");
	}

	string import_format = "dump";
	if (argc > 0) import_format = cl.arguments[0];

	string export_format = "dump";
	if (argc > 1) export_format = cl.arguments[1];

	if (cl.verbose)
	{
		cout << "converting from " << import_format
		     << " to " << export_format << endl;
	}

	Modules modules;
	auto_ptr<Plugin> import_plugin = modules.load(import_format);
	auto_ptr<Plugin> export_plugin = modules.load(export_format);

	import_plugin->unserialize(ks);
	export_plugin->serialize(ks);

	return 0;
}

ConvertCommand::~ConvertCommand()
{}

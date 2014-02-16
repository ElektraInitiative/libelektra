#include <file.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>
#include <print.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

FileCommand::FileCommand()
{}

int FileCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument ("Need one argument");

	KeySet conf;
	Key x(cl.arguments[0], KEY_END);
	if (!x.isValid())
	{
		throw invalid_argument(cl.arguments[0] + " is not an valid keyname");
	}

	kdb.get(conf, x);
	cout << x.getString() << endl;

	if (!cl.noNewline)
	{
		cout << endl;
	}

	return 0;
}

FileCommand::~FileCommand()
{}

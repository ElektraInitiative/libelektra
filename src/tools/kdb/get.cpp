#include <get.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

GetCommand::GetCommand()
{}

int GetCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument ("Need one argument");

	KeySet conf;
	Key x(cl.arguments[0], KEY_END);
	if (!x.isValid())
	{
		throw invalid_argument(cl.arguments[0] + " is not an valid keyname");
	}

	kdb.get(conf, x);
	Key k = conf.lookup(x);

	int ret = 0;

	if (k)
	{
		cout << k.getString();
	}
	else
	{
		cerr << "Did not find key";
		ret = 1;
	}

	if (!cl.noNewline)
	{
		cout << endl;
	}

	printWarnings(cerr, x);
	printError(cerr, x);

	return ret;
}

GetCommand::~GetCommand()
{}

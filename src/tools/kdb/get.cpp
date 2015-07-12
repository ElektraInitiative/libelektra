#include <get.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

GetCommand::GetCommand() : kdb(root)
{}

int GetCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument ("Need one argument");

	KeySet conf;
	root.setName(cl.arguments[0]);
	if (!root.isValid())
	{
		throw invalid_argument(cl.arguments[0] + " is not an valid keyname");
	}

	kdb.get(conf, root);
	Key k = conf.lookup(root);

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

	printWarnings(cerr, root);
	printError(cerr, root);

	return ret;
}

GetCommand::~GetCommand()
{}

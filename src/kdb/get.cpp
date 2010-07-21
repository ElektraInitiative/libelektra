#include <get.hpp>

#include <kdb>

#include <iostream>

using namespace std;
using namespace kdb;

GetCommand::GetCommand()
{}

int GetCommand::execute(int argc, char** argv)
{
	if (argc != 3)
	{
		cerr << "Please provide a name" << endl;
		cerr << "Usage: get <name>" << endl;
		return 1;
	}

	KeySet conf;
	Key x(argv[2], KEY_END);
	if (!x.isValid())
	{
		cerr << "Argument given is not a valid keyname" << endl;
		return 1;
	}
	kdb.get(conf, x);
	Key k = conf.lookup(x);

	if (!k)
	{
		cerr << "Did not find key" << endl;
		return 1;
	}

	cout << k.getString() << endl;

	return 0;
}

GetCommand::~GetCommand()
{}

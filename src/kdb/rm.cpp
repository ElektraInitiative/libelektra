#include <rm.hpp>

#include <kdb>

#include <iostream>

using namespace std;
using namespace kdb;

RemoveCommand::RemoveCommand()
{}

int RemoveCommand::execute(int argc, char** argv)
{
	if (argc != 3)
	{
		cerr << "Please provide a name" << endl;
		cerr << "Usage: rm <name>" << endl;
		return 1;
	}

	KeySet conf;
	Key x(argv[2], KEY_END);
	if (!x)
	{
		cerr << "Argument given is not a valid keyname" << endl;
		return 1;
	}
	kdb.get(conf, x);
	Key k = conf.lookup(x, KDB_O_POP);

	if (!k)
	{
		cerr << "Did not find key" << endl;
		return 1;
	}

	Key n;
	kdb.set(conf, n);

	return 0;
}

RemoveCommand::~RemoveCommand()
{}

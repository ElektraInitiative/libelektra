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
	kdb.get(conf, Key(argv[2], KEY_END));
	Key k = conf.lookup(argv[2]);

	cout << k.getString() << endl;

	return 0;
}

GetCommand::~GetCommand()
{}

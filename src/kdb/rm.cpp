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

	std::string command = argv[1];
	if (command == "rm")
	{
		KeySet k ( conf.cut (x));

		if (k.size() == 0)
		{
			cerr << "Did not find any key" << endl;
			return 1;
		}
	} else {
		// do recursive removing
		KeySet ks = conf.cut (x);

		if (ks.size() == 0)
		{
			cerr << "Did not find any key" << endl;
			return 1;
		}
	}

	Key n;
	kdb.set(conf, n);

	return 0;
}

RemoveCommand::~RemoveCommand()
{}

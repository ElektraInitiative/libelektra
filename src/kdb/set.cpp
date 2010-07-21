#include <set.hpp>

#include <kdb>

#include <iostream>

using namespace std;
using namespace kdb;

SetCommand::SetCommand()
{}

int SetCommand::execute(int argc, char**argv)
{
	if (argc != 4)
	{
		cerr << "Please provide a name and a value to set" << endl;
		cerr << "Usage: set <name> <value>" << endl;
		return 1;
	}

	std::string name = argv[2];
	std::string value = argv[3];

	KeySet conf;
	Key k(name, KEY_END);
	try {
		kdb.get(conf, k);
		printWarnings(k);
	} catch (...)
	{
		printError(k);
		cerr << "kdb get failed, but still resume" << endl;
	}
	Key key = conf.lookup(name);

	if (!key)
	{
		cout << "create a new key with " << name << " and " << value << endl;
		key = Key(name, KEY_VALUE, value.c_str(), KEY_END);
		if (!key.isValid())
		{
			cerr << "no valid name supplied" << endl;
			return 1;
		}
		conf.append(key);
	} else {
		cout << "Set string to " << value << endl;
		key.setString(value);
	}
	Key n;
	kdb.set(conf, n);
	printWarnings(n);

	return 0;
}

SetCommand::~SetCommand()
{}

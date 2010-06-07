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
	try {
		kdb.get(conf, Key(name, KEY_END));
	} catch (KDBException const& e)
	{
		cerr << "kdb get failed, but still resume" << endl;
	}
	Key key = conf.lookup(name);

	if (!key)
	{
		cout << "create a new key with " << name << " and " << value << endl;
		key = Key(name, KEY_VALUE, value.c_str(), KEY_END);
		conf.append(key);
	} else {
		cout << "Set string to " << value << endl;
		key.setString(value);
	}
	kdb.set(conf, Key(static_cast<ckdb::Key*>(0)));

	return 0;
}

SetCommand::~SetCommand()
{}

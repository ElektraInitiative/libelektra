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
	kdb.get(conf, Key(name, KEY_END));
	Key key;
	try {
		key = conf.lookup(name);

		cout << "Set string to " << value << endl;
		key.setString(value);
	} catch (kdb::KeySetNotFound const& e) {
		cout << "create a new key with " << name << " and " << value << endl;
		key = Key(name, KEY_VALUE, value.c_str(), KEY_END);
		conf.append(key);
	}
	kdb.set(conf, Key(static_cast<ckdb::Key*>(0)));

	return 0;
}

SetCommand::~SetCommand()
{}

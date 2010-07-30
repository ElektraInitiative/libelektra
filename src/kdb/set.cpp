#include <set.hpp>

#include <kdb>

#include <iostream>

using namespace std;
using namespace kdb;

SetCommand::SetCommand()
{}

int SetCommand::execute(int argc, char**argv)
{
	if (argc != 3 && argc != 4)
	{
		cerr << "Please provide a name and a value to set" << endl;
		cerr << "Usage: set <name> [<value>]" << endl;
		cerr << "If no value is given, it will be set to a null-value" << endl;
		cerr << "To get an empty value you need to quote like \"\" (depending on shell)" << endl;
		return 1;
	}

	std::string name = argv[2];

	bool nullValue = false;
	if (argc == 3) nullValue = true;

	std::string value;

	if (!nullValue) value = argv[3];

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
		key = Key(name, KEY_END);
		if (!nullValue) key.setString(value);
		if (!key.isValid())
		{
			cerr << "no valid name supplied" << endl;
			return 1;
		}
		conf.append(key);
	} else {
		cout << "Set string to " << value << endl;
		if (!nullValue) key.setString(value);
	}
	Key n;
	kdb.set(conf, n);
	printWarnings(n);

	return 0;
}

SetCommand::~SetCommand()
{}

#include <meta.hpp>

#include <kdb>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

MetaCommand::MetaCommand()
{}

int MetaCommand::execute(int argc, char** argv)
{
	string prog = argv[0];
	string command = argv[1];
	if (command == "meta-get" && argc < 4)
	{
		cerr << "Usage: " << prog << " " << command << " <key-name> <meta-name>" << endl;
		return 1;
	} else if (command == "meta-set" && argc < 5)
	{
		cerr << "Usage: " << prog << " " << command << " <key-name> <meta-name> <meta-value>" << endl;
		return 1;
	}
	string keyname = argv[2];
	string metaname = argv[3];

	KeySet conf;
	kdb.get(conf, Key(keyname, KEY_END));
	Key k = conf.lookup(keyname);

	if (command == "meta-get")
	{
		cout << k.getMeta<string>(metaname) << endl;
	} else if (command == "meta-set")
	{
		std::string metavalue = argv[4];
		if (metaname == "atime" || metaname == "mtime" || metaname == "ctime")
		{
			stringstream str (metavalue);
			time_t t;
			str >> t;
			if (!str.good()) throw "conversion failure";
			k.setMeta<time_t> (metaname, t);
		} else {
			k.setMeta<string> (metaname, metavalue);
		}

		kdb.set(conf, Key(keyname, KEY_END));
	}

	return 0;
}

MetaCommand::~MetaCommand()
{}

#include <fstab.hpp>

#include <kdb.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

FstabCommand::FstabCommand()
{}

int FstabCommand::execute(int argc, char** argv)
{
	string prog = argv[0];
	string command = argv[1];
	if (argc != 7)
	{
		cerr << "Usage: " << prog << " " << command << " <key-name> <device> <mpoint> <type> <options>" << endl;
		return 1;
	}
	string keyname = argv[2];

	KeySet conf;
	Key parentKey(keyname, KEY_END);
	kdb.get(conf, parentKey);
	Key k = conf.lookup(keyname);

	if (!k)
	{
		k = Key(keyname, KEY_END);
		conf.append (k);
	}

	if (!k.isValid())
	{
		cout << "Could not create key" << endl;
		return 1;
	}

	kdb::KeySet config( 20,
		*kdb::Key (keyname + "/ZZZNewFstabName",
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/device",
			KEY_VALUE, argv[3],
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/mpoint",
			KEY_VALUE, argv[4],
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/type",
			KEY_VALUE, argv[5],
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/options",
			KEY_VALUE, argv[6],
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/dumpfreq",
			KEY_VALUE, "0",
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/passno",
			KEY_VALUE, "0",
			KEY_END),
		KS_END);

	conf.append(config);

	conf.rewind();
	while (Key k = conf.next())
	{
		cout << k.getName() << " " << k.getString() << endl;
	}

	kdb.set(conf,parentKey);

	return 0;
}

FstabCommand::~FstabCommand()
{}

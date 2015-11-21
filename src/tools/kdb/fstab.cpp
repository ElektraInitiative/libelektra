/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <fstab.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

FstabCommand::FstabCommand()
{}

int FstabCommand::execute(Cmdline const& cl)
{
	int argc = cl.arguments.size();
	if (argc != 5 && argc != 6 && argc != 7)
	{
		throw invalid_argument("number of arguments not correct, need 5, 6 or 7");
	}

	string keyname = cl.arguments[0];

	KeySet conf;
	Key parentKey(keyname, KEY_END);
	kdb.get(conf, parentKey);
	printWarnings(cerr, parentKey);
	Key k = conf.lookup(keyname);

	if (!k)
	{
		k = Key(keyname, KEY_END);
		conf.append (k);
	}

	if (!k.isValid())
	{
		throw invalid_argument("keyname is not valid");
	}

	string dumpfreq = "0";
	if (argc >= 6)
	{
		dumpfreq = cl.arguments[5].c_str();
	}

	string passno = "0";
	if (argc >= 7)
	{
		passno = cl.arguments[6].c_str();
	}

	kdb::KeySet config( 20,
		*kdb::Key (keyname + "/ZZZNewFstabName",
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/device",
			KEY_VALUE, cl.arguments[1].c_str(),
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/mpoint",
			KEY_VALUE, cl.arguments[2].c_str(),
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/type",
			KEY_VALUE, cl.arguments[3].c_str(),
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/options",
			KEY_VALUE, cl.arguments[4].c_str(),
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/dumpfreq",
			KEY_VALUE, dumpfreq.c_str(),
			KEY_END),
		*kdb::Key (keyname + "/ZZZNewFstabName/passno",
			KEY_VALUE, passno.c_str(),
			KEY_END),
		KS_END);

	conf.append(config);

	if (cl.verbose)
	{
		cout << conf;
	}

	kdb.set(conf,parentKey);
	printWarnings(cerr, parentKey);

	return 0;
}

FstabCommand::~FstabCommand()
{}

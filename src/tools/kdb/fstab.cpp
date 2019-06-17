/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <fstab.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>

#include <iostream>
#include <string>

using namespace std;
using namespace kdb;

FstabCommand::FstabCommand ()
{
}

int FstabCommand::execute (Cmdline const & cl)
{
	int argc = cl.arguments.size ();
	if (argc != 5 && argc != 6 && argc != 7)
	{
		throw invalid_argument ("number of arguments not correct, need 5, 6 or 7");
	}

	KeySet conf;
	Key parentKey = cl.createKey (0);
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);
	Key k = conf.lookup (parentKey);

	if (!k)
	{
		k = cl.createKey (0);
		conf.append (k);
	}

	std::string keyname = k.getName ();

	string dumpfreq = "0";
	if (argc >= 6)
	{
		dumpfreq = cl.arguments[5].c_str ();
	}

	string passno = "0";
	if (argc >= 7)
	{
		passno = cl.arguments[6].c_str ();
	}

	kdb::KeySet config (20, *kdb::Key (keyname + "/ZZZNewFstabName", KEY_END),
			    *kdb::Key (keyname + "/ZZZNewFstabName/device", KEY_VALUE, cl.arguments[1].c_str (), KEY_END),
			    *kdb::Key (keyname + "/ZZZNewFstabName/mpoint", KEY_VALUE, cl.arguments[2].c_str (), KEY_END),
			    *kdb::Key (keyname + "/ZZZNewFstabName/type", KEY_VALUE, cl.arguments[3].c_str (), KEY_END),
			    *kdb::Key (keyname + "/ZZZNewFstabName/options", KEY_VALUE, cl.arguments[4].c_str (), KEY_END),
			    *kdb::Key (keyname + "/ZZZNewFstabName/dumpfreq", KEY_VALUE, dumpfreq.c_str (), KEY_END),
			    *kdb::Key (keyname + "/ZZZNewFstabName/passno", KEY_VALUE, passno.c_str (), KEY_END), KS_END);

	conf.append (config);

	if (cl.verbose)
	{
		cout << conf;
	}

	kdb.set (conf, parentKey);
	printWarnings (cerr, parentKey, cl.verbose, cl.debug);

	return 0;
}

FstabCommand::~FstabCommand ()
{
}

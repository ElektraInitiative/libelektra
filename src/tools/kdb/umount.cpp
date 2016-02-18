/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <umount.hpp>

#include <backends.hpp>
#include <cmdline.hpp>
#include <kdb.hpp>

#include <iostream>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

UmountCommand::UmountCommand ()
{
}

int UmountCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1)
		throw invalid_argument ("1 argument required");

	KeySet conf;
	Key parentKey (Backends::mountpointsPath, KEY_END);
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey);

	std::string name = cl.createKey (0).getName ();

	if (cl.verbose)
		Backends::findBackend (name, conf, true);

	if (Backends::umount (name, conf) == 0)
	{
		cerr << "Mountpoint " << name << " does not exist" << endl;
		return 1;
	}

	kdb.set (conf, parentKey);

	return 0;
}

UmountCommand::~UmountCommand ()
{
}

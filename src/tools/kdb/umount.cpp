#include <umount.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>
#include <backends.hpp>

#include <iostream>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

UmountCommand::UmountCommand()
{}

int UmountCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument("1 argument required");

	KeySet conf;
	Key parentKey (Backends::mountpointsPath, KEY_END);
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey);

	if (cl.verbose) Backends::findBackend(cl.arguments[0], conf, true);

	if (Backends::umount(cl.arguments[0], conf) == 0)
	{
		cerr << "Mountpoint " << cl.arguments[0] << " does not exist" << endl;
		return 1;
	}

	kdb.set(conf, parentKey);

	return 0;
}

UmountCommand::~UmountCommand()
{}

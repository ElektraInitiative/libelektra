#include <umount.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

UmountCommand::UmountCommand()
{}

int UmountCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument("1 argument required");

	std::string const & mountpoints_name =
		"system/elektra/mountpoints";

	std::string const & backend_name = cl.arguments[0];

	KeySet conf;
	std::string key_name = mountpoints_name + "/"  + backend_name;
	Key x(key_name, KEY_END);
	if (!x)
	{
		throw invalid_argument(key_name + " is not a valid keyname");
	}

	{
		Key parentKey(mountpoints_name, KEY_END);
		kdb.get(conf, parentKey);
		printWarnings (cerr, parentKey);
	}

	KeySet ks = conf.cut (x);

	if (ks.size() == 0)
	{
		cerr << "Backend " << backend_name << " does not exist"  << endl;
		return 1;
	}

	{
		Key parentKey(mountpoints_name, KEY_END);
		kdb.set(conf, parentKey);
		printWarnings (cerr, parentKey);
	}

	return 0;
}

UmountCommand::~UmountCommand()
{}

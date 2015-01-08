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

int UmountCommand::deleteByMountPath(KeySet& conf, std::string const & mountPath)
{
	Backends::BackendInfoVector mtab = Backends::getBackendInfo (conf);

	std::string backendName;
	for (Backends::BackendInfoVector::const_iterator it = mtab.begin (); it != mtab.end (); ++it)
	{
		if (it->mountpoint == mountPath)
		{
			Key x(Backends::getBasePath (mountPath), KEY_END);
			KeySet ks = conf.cut (x);
			return ks.size();
		}
	}

	return 0;
}

int UmountCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument("1 argument required");

	KeySet conf;
	Key parentKey (Backends::mountpointsPath, KEY_END);
	kdb.get (conf, parentKey);
	printWarnings (cerr, parentKey);

	if (deleteByMountPath(conf, cl.arguments[0]) == 0)
	{
		cerr << "Mountpoint " << cl.arguments[0] << " does not exist" << endl;
		return 1;
	}

	kdb.set(conf, parentKey);

	return 0;
}

UmountCommand::~UmountCommand()
{}

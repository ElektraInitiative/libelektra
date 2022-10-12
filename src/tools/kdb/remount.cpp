/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <remount.hpp>

#include <backend.hpp>
#include <backends.hpp>
#include <cmdline.hpp>
#include <helper/keyhelper.hpp>
#include <kdb.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::helper;

RemountCommand::RemountCommand ()
{
}


void RemountCommand::getExistingMountpoint (Cmdline const & cl)
{
	std::string search = cl.arguments[2];
	BackendInfo bi = Backends::findBackend (search, mountConf);

	if (bi.mountpoint.empty ())
	{
		throw invalid_argument ("could not find the mountpoint \"" + search + "\"");
	}

	existingMountpoint = bi.mountpoint;
}

void RemountCommand::cloneMountpoint (Cmdline const & cl)
{
	Key existingParent (Backends::getBasePath (existingMountpoint), KEY_END);
	Key newParent (Backends::getBasePath (mp), KEY_END);

	KeySet existingBackend = mountConf.cut (existingParent);
	mountConf.append (existingBackend);
	KeySet newBackend (existingBackend.size (), KS_END);
	string configPath = newParent.getName () + "/config/path";
	string mpPath = newParent.getName () + "/mountpoint";

	for (Key current : existingBackend)
	{
		Key newKey = rebaseKey (current, existingParent, newParent);
		newBackend.append (newKey);

		if (newKey.getName () == mpPath)
		{
			newKey.setString (mp);
		}

		if (newKey.getName () == configPath)
		{
			newKey.setString (cl.arguments[0]);
		}
	}

	mountConf.append (newBackend);
}

int RemountCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 3) throw invalid_argument ("3 argument required");

	readMountConf (cl);
	getExistingMountpoint (cl);
	getMountpoint (cl);
	cloneMountpoint (cl);
	askForConfirmation (cl);
	doIt ();

	return 0;
}

RemountCommand::~RemountCommand ()
{
}

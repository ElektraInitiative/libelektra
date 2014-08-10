#include <remount.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>
#include <backend.hpp>
#include <backends.hpp>
#include <helper/keyhelper.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::helper;

RemountCommand::RemountCommand()
{}


void RemountCommand::getExistingMountpoint(Cmdline const & cl)
{
	string existingBackend;
	Backends::BackendInfoVector mtab = Backends::getBackendInfo (mountConf);
	bool byPath = cl.arguments[2].find ("/") != string::npos;
	bool backendFound = false;
	for (Backends::BackendInfoVector::const_iterator it = mtab.begin (); it != mtab.end (); ++it)
	{
		if (byPath)
		{
			if (it->mountpoint == cl.arguments[2])
			{
				backendFound = true;
			}
		}
		else
		{
			if (it->name == cl.arguments[2])
			{
				backendFound = true;
			}
		}

		if (backendFound)
		{
			existingName = it->name;
			break;
		}
	}

	if (!backendFound) throw invalid_argument ("the mount " + existingBackend + " does not exist");
}

void RemountCommand::cloneMountpoint(Cmdline const & cl)
{
	Key existingParent (string(Backends::mountpointsPath) + "/" + existingName, KEY_END);
	Key newParent (string(Backends::mountpointsPath) + "/" + name, KEY_END);
	kdb::KDB kdb (existingParent);
	KeySet existingBackend = mountConf.cut(existingParent);
	mountConf.append(existingBackend);
	KeySet newBackend(existingBackend.size(), KS_END);
	string configPath = newParent.getName() + "/config/path";
	existingBackend.rewind();
	while (Key current = existingBackend.next())
	{
		Key newKey = rebaseKey (current, existingParent, newParent);
		newBackend.append(newKey);

		if (newKey.getBaseName() == "mountpoint")
		{
			newKey.setString(mp);
		}

		if (newKey.getName() == configPath)
		{
			newKey.setString(cl.arguments[0]);
		}
	}

	mountConf.append(newBackend);
}

int RemountCommand::execute(Cmdline const & cl)
{
	if (cl.arguments.size() != 3) throw invalid_argument("3 argument required");

	readMountConf();
	getExistingMountpoint(cl);
	fixRootKey(cl);
	getName(cl);
	getMountpoint(cl);
	cloneMountpoint(cl);
	askForConfirmation(cl);
	doIt();

	return 0;
}

RemountCommand::~RemountCommand()
{}



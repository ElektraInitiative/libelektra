#include <backends.hpp>

#include <algorithm>

namespace kdb
{

namespace tools
{

#if __cplusplus > 199711L
using std::move;
#else
#define move(x) x
#endif

/**
 * @brief give info about current mounted backends
 *
 * @param mountConf a keyset that contains everything below
 * Backends::mountpointsPath
 *
 * @return an vector of information about mounted backends
 */
Backends::BackendInfoVector Backends::getBackendInfo(KeySet mountConf)
{
	std::vector<BackendInfo> ret;
	Key rootKey (Backends::mountpointsPath, KEY_END);
	Key cur;

	mountConf.rewind();
	while ((cur = mountConf.next()))
	{
		if (cur.isDirectBelow(rootKey))
		{
			BackendInfo bi;

			Key path = mountConf.lookup (cur.getName() + "/config/path");
			if (path)
			{
				bi.path = path.getString();
			}
			Key mp = mountConf.lookup (cur.getName() + "/mountpoint");
			if (mp)
			{
				bi.mountpoint = mp.getString();
			}
			bi.name = cur.getBaseName();

			ret.push_back(bi);
		}
	}
	return move(ret);
}

/**
 * @brief returns the escaped name for backends
 *
 * @param name to escape
 *
 * @return the properly escaped name
 */
std::string Backends::escapeName(std::string name)
{
	std::string configPath;

	 // take care of double slashes, . and ..
	Key n("user/"+name, KEY_END);
	if (name == "/") return "\\/";
	if (name[0] == '/') configPath+="\\/";

	name = n.getName().substr(5);
	// escape / and _
	for (size_t i=0; i<name.length(); ++i)
	{
		if (name[i] == '/')
		{
			configPath += "\\/";
		} else if (name[i] == '\\') {
			configPath += "\\\\";
		} else {
			configPath += name[i];
		}
	}
	return configPath;
}

/**
 * @brief returns the config base path of a mounted backend
 * below system/elektra/mountpoints
 *
 * @param mp the mountpoint (name will be derived from it)
 *
 * @return the properly prefixed and escaped name
 */
std::string Backends::getConfigBasePath(std::string mp)
{
	std::string configPath = Backends::mountpointsPath;
	configPath += "/";
	configPath += Backends::escapeName(mp);
	configPath += "/config";
	return configPath;
}

/**
 * @brief Below this path is the mountConf
 */
const char *Backends::mountpointsPath = "system/elektra/mountpoints";

}

}

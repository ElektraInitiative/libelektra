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
 * @brief returns the config base path of a mounted backend
 * below system/elektra/mountpoints
 *
 * @param mp the mountpoint (name will be derived from it)
 *
 * @return the properly prefixed and escaped name
 */
std::string Backends::getConfigBasePath(std::string mp)
{
	Key kmp(mp, KEY_CASCADING_NAME, KEY_END); // canonify name
	Key k(Backends::mountpointsPath, KEY_END);
	k.addBaseName(kmp.getName()); // escape name
	k.addBaseName("config");
	return k.getName();
}

/**
 * @brief Below this path is the mountConf
 */
const char *Backends::mountpointsPath = "system/elektra/mountpoints";

}

}

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
 * @brief Unmount a backend by given mountPath
 *
 * @param mountPath the given mountpoint (for backwards compatibility
 * names old-style / and _ are accepted if no modern-style
 * mountpoint was found, too)
 *
 * @retval true if something was done
 * @retval false if nothing was done (but also no error)
 */
bool Backends::umount(std::string const & mountPath, KeySet & mountConf)
{
	if (mountPath.empty()) return false;

	Backends::BackendInfoVector mtab = Backends::getBackendInfo (mountConf);

	Key kmp(Backends::getBasePath(mountPath), KEY_END);

	// search for proper mountname:
	for (Backends::BackendInfoVector::const_iterator it = mtab.begin (); it != mtab.end (); ++it)
	{
		if (it->mountpoint == kmp.getBaseName())
		{
			Key x(Backends::mountpointsPath, KEY_END);;
			x.addBaseName(it->name);
			mountConf.cut(x);
			return true;
		}
	};

	// fall back to compatibility pre 0.8.11 mountnames
	// May umount wrong things, so its done as extra step so that
	// it will never happen if something desired is present.
	std::string soldMountpoint = mountPath;
	std::replace(soldMountpoint.begin(), soldMountpoint.end(), '_', '/');
	Key koldMountpoint("user/" + soldMountpoint, KEY_END);
	std::string omp = koldMountpoint.getName();
	std::string oldMountpoint(omp.begin()+4, omp.end());
	if (soldMountpoint.at(0) != '/') oldMountpoint.erase(0,1); // fix non-cascading
	if (koldMountpoint.getName() == "user") oldMountpoint = "/"; // fix root
	for (Backends::BackendInfoVector::const_iterator it = mtab.begin (); it != mtab.end (); ++it)
	{
		if (it->mountpoint == oldMountpoint)
		{
			Key x(Backends::mountpointsPath, KEY_END);;
			x.addBaseName(it->name);
			mountConf.cut(x);
			return true;
		}
	};

	return false;
}

/**
 * @brief returns the base path of a mounted backend
 * below system/elektra/mountpoints
 *
 * @param mp the mountpoint (name will be derived from it)
 *
 * @return the properly prefixed and escaped name
 */
std::string Backends::getBasePath(std::string mp)
{
	Key k(Backends::mountpointsPath, KEY_END);
	Key kmp(mp, KEY_CASCADING_NAME, KEY_END); // canonify name
	k.addBaseName(kmp.getName()); // escape name
	return k.getName();
}

/**
 * @brief Below this path is the mountConf
 */
const char *Backends::mountpointsPath = "system/elektra/mountpoints";

}

}

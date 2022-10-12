/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <backends.hpp>

#include <algorithm>
#include <iostream>

namespace kdb
{

namespace tools
{

/**
 * @brief give info about current mounted backends
 *
 * @param mountConf a keyset that contains everything below
 * Backends::mountpointsPath
 *
 * @return an vector of information about mounted backends
 */
Backends::BackendInfoVector Backends::getBackendInfo (KeySet mountConf)
{
	std::vector<BackendInfo> ret;
	Key rootKey (Backends::mountpointsPath, KEY_END);

	for (Key cur : mountConf)
	{
		if (cur.isDirectBelow (rootKey))
		{
			BackendInfo bi;

			Key path = mountConf.lookup (cur.getName () + "/definition/path");
			if (path)
			{
				bi.path = path.getString ();
			}
			bi.mountpoint = cur.getBaseName ();

			ret.push_back (bi);
		}
	}
	return ret;
}

/**
 * @brief Find a backend in the given name
 *
 * @param mountPath the given backend name to find
 *
 * For backwards compatibility old-style names containing _ instead of escaped /
 * are accepted if no modern-style mountpoint is found.
 *
 * @param mountConf the configuration to search (should contain keys
 * below mountpointsPath to find something)
 *
 * @return the found backend or an empty BackendInfo if nothing found
 *         (with empty strings)
 */
BackendInfo Backends::findBackend (std::string const & mountPath, KeySet mountConf, bool verbose)
{
	BackendInfo ret;
	if (mountPath.empty ()) return ret;

	Backends::BackendInfoVector mtab = Backends::getBackendInfo (mountConf);

	Key kmp (Backends::getBasePath (mountPath), KEY_END);

	// search for proper mountname:
	for (Backends::BackendInfoVector::const_iterator it = mtab.begin (); it != mtab.end (); ++it)
	{
		if (verbose) std::cout << "compare: " << it->mountpoint << " with " << kmp.getBaseName () << std::endl;
		if (it->mountpoint == kmp.getBaseName ())
		{
			return *it;
		}
	};

	// fall back to compatibility pre 0.8.11 mountnames
	// May umount wrong things, so its done as extra step so that
	// it will never happen if something desired is present.
	std::string soldMountpoint = mountPath;
	std::replace (soldMountpoint.begin (), soldMountpoint.end (), '_', '/');
	Key koldMountpoint ("user:/" + soldMountpoint, KEY_END);
	std::string omp = koldMountpoint.getName ();
	std::string oldMountpoint (omp.begin () + 4, omp.end ());
	if (soldMountpoint.at (0) != '/') oldMountpoint.erase (0, 1); // fix non-cascading
	if (koldMountpoint.getName () == "user") oldMountpoint = "/"; // fix root
	for (Backends::BackendInfoVector::const_iterator it = mtab.begin (); it != mtab.end (); ++it)
	{
		if (verbose) std::cout << "fallback compare: " << it->mountpoint << " with " << oldMountpoint << std::endl;
		if (it->mountpoint == oldMountpoint)
		{
			return *it;
		}
	}
	return ret;
}

/**
 * @brief Unmount a backend by given mountPath
 *
 * @param mountPath the given mountpoint
 *
 * Uses findBackend() to locate the backend.
 *
 * @retval true if something was done
 * @retval false if nothing was done (but also no error)
 */
bool Backends::umount (std::string const & mountPath, KeySet & mountConf)
{
	BackendInfo bi = Backends::findBackend (mountPath, mountConf);
	if (!bi.mountpoint.empty ())
	{
		Key x (Backends::mountpointsPath, KEY_END);
		x.addBaseName (bi.mountpoint);
		mountConf.cut (x);
		return true;
	}

	return false;
}

/**
 * @brief returns the base path of a mounted backend
 * below system:/elektra/mountpoints
 *
 * @param mp the mountpoint (name will be derived from it)
 *
 * @return the properly prefixed and escaped name
 */
std::string Backends::getBasePath (std::string mp)
{
	Key k (Backends::mountpointsPath, KEY_END);
	Key kmp (mp, KEY_END);		// canonify name
	k.addBaseName (kmp.getName ()); // escape name
	return k.getName ();
}

/**
 * @brief Below this path is the mountConf
 */
const char * Backends::mountpointsPath = "system:/elektra/mountpoints";
} // namespace tools
} // namespace kdb

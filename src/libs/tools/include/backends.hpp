/**
 * @file
 *
 * @brief Allows one to list all available backends
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef TOOLS_BACKENDS_HPP
#define TOOLS_BACKENDS_HPP

#include <string>
#include <vector>

#include <keyset.hpp>
#include <toolexcept.hpp>

namespace kdb
{

namespace tools
{

/**
 * @brief Info about a backend
 */
struct BackendInfo
{
	std::string mountpoint; ///< where the backend is mounted
	std::string path;	///< the configuration file path to this backend
};

/**
 * @brief Allows one to list backends
 */
class Backends
{
public:
	typedef std::vector<BackendInfo> BackendInfoVector;

	static BackendInfoVector getBackendInfo (KeySet mountConf);

	static BackendInfo findBackend (std::string const & backend, KeySet mountConf, bool verbose = false);

	static bool umount (std::string const & backend, KeySet & mountConf);

	static std::string getBasePath (std::string name);

	static const char * const mountpointsPath;
};
} // namespace tools
} // namespace kdb

#endif

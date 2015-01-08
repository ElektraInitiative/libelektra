/**
 * \file
 *
 * \brief Allows one to list all available backends
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef TOOLS_BACKENDS_HPP
#define TOOLS_BACKENDS_HPP

#include <vector>
#include <string>

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
	std::string name;
	std::string mountpoint;
	std::string path;
};

/**
 * @brief Allows one to list backends
 */
class Backends
{
public:
	typedef std::vector<BackendInfo> BackendInfoVector;

	static BackendInfoVector getBackendInfo(KeySet mountConf);

	static std::string getBasePath(std::string name);

	static const char * mountpointsPath;
};

}

}

#endif

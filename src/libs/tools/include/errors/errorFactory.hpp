//
// Created by flo on 1/16/22.
//

#ifndef ELEKTRA_ERRORFACTORY_HPP
#define ELEKTRA_ERRORFACTORY_HPP

#include "error.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

/* make sure to delete (free) the returned Errors */
class ErrorFactory
{
public:
	/* takes one of the ELEKTRA_ERROR_* constants (e.g. ELEKTRA_ERROR_OUT_OF_MEMORY)
	 * from /src/include/kdberrors.h as a parameter */
	static Error * create (const std::string & type, const std::string & reason, const std::string & module,
			       const std::string & file, const std::string & mountPoint, const std::string & configFile, kdb::long_t line);

	/* Creates an Error object including Warnings as provided by the given key. */
	static Error * fromKey(kdb::Key key);

	/* checks if a code and description fit together */
	static bool checkErrorCodeDesc(const std::string & code, const std::string & description);
};

} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERRORFACTORY_HPP

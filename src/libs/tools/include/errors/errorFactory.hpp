//
// Created by flo on 1/16/22.
//

#ifndef ELEKTRA_ERRORFACTORY_HPP
#define ELEKTRA_ERRORFACTORY_HPP

#include "./error.hpp"

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
	ErrorFactory () = delete;
	~ErrorFactory () = delete;

	/* takes one of the ELEKTRA_ERROR_* constants (e.g. ELEKTRA_ERROR_OUT_OF_MEMORY)
	 * from /src/include/kdberrors.h as a parameter */
	static Error * create (const std::string & type, const std::string & description, const std::string & reason,
			       const std::string & module, const std::string & file, const std::string & mountPoint,
			       const std::string & configFile, kdb::long_t line);

	/**
	 * @brief Create an error from a given key
	 *
	 * Reads meta-keys of given key to find error and warnings meta-keys. If no error exists a PureWarningError is created that contains
	 * the key's warnings.
	 *
	 * @param key the key that has the error and warnings
	 *
	 * @return the error with warnings
	 */
	static Error * fromKey (kdb::Key key);
};

} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERRORFACTORY_HPP

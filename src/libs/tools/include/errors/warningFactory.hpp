//
// Created by flo on 1/16/22.
//

#ifndef ELEKTRA_WARNINGFACTORY_HPP
#define ELEKTRA_WARNINGFACTORY_HPP

#include "./warning.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

class WarningFactory
{
public:
	WarningFactory () = delete;
	~WarningFactory () = delete;

	/* takes one of the ELEKTRA_WARNING_* constants (e.g. ELEKTRA_WARNING_OUT_OF_MEMORY)
	 * from /src/include/kdberrors.h as a parameter */
	/* You must delete the object that is returned by this function! */
	static Warning * create (const std::string & type, const std::string & description, const std::string & reason,
				 const std::string & module, const std::string & file, const std::string & mountPoint,
				 const std::string & configFile, kdb::long_t line);
};

} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_WARNINGFACTORY_HPP

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_ELEKTRADIFFEXCEPT_HPP
#define ELEKTRA_ELEKTRADIFFEXCEPT_HPP

#include <keyexcept.hpp>

namespace kdb
{
class ElektraDiffNullException : public Exception
{
public:
	virtual const char * what () const throw ()
	{
		return "You passed a NULL pointer as underlying diff";
	}
};

}

#endif // ELEKTRA_ELEKTRADIFFEXCEPT_HPP

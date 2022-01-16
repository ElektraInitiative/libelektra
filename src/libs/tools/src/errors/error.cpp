


#include "errors/error.hpp"
#include <iostream>

namespace kdb
{
namespace tools
{
namespace errors
{

void Error::addWarning (Warning *warning)
{
	warnings.push_back (warning);
}

/* getters */
kdb::long_t Error::warningCount ()
{
	return warnings.size();
}

} // namespace errors
} // namespace tools
} // namespace kdb
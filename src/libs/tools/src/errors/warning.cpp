
#include "./errors/warning.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

bool Warning::compare (const BaseNotification & other) const
{
	/* comparison of data fields is done by operator== in BaseNotification class */
	return (dynamic_cast<const Warning *> (&other)) ? true : false;
}

} // namespace errors
} // namespace tools
} // namespace kdb

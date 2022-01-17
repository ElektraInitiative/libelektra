
#include "errors/warning.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

/**
 * @brief Compare warnings
 *
 * The comparison of data fields is done by operator== in the BaseNotification class.
 * This function compares the type of BaseNotification in addition to the notification fields.
 *
 * @param other the notification to compare to
 *
 * @return true if objects are equal
 */
bool Warning::compare(const BaseNotification& other) const
{
	/* comparison of data fields is done by operator== in BaseNotification class */
	return (dynamic_cast<const Warning *> (&other)) ? true : false;
}

} // namespace errors
} // namespace tools
} // namespace kdb
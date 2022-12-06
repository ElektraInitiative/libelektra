#ifndef ELEKTRA_BASENOTIFICATION_HPP
#define ELEKTRA_BASENOTIFICATION_HPP

#include <elektra/kdbtypes.h>
#include <key.hpp>
#include <string>
#include <utility>

namespace kdb
{
namespace tools
{
namespace errors
{
/* Common abstract class for warnings and errors.
 * Because warning and errors share the same data members,
 * a method can accept a BaseNotification argument and
 * the caller can create an Error or a Warning
 * based on the provided object. */
class BaseNotification
{
public:
	/* constructor */
	BaseNotification (std::string description, std::string reason, std::string module, std::string file, std::string mountPoint,
			  std::string configFile, kdb::long_t line);

	/* setters */
	void setData (const std::string & description, const std::string & reason, const std::string & module, const std::string & file,
		      const std::string & mountPoint, const std::string & configFile, kdb::long_t line);

	/* get references (for setting and getting member values) */
	std::string & reason ();
	std::string & module ();
	std::string & file ();
	std::string & mountPoint ();
	std::string & configFile ();
	kdb::long_t & line ();
	const std::string & description () const;
	const std::string & reason () const;
	const std::string & module () const;
	const std::string & file () const;
	const std::string & mountPoint () const;
	const std::string & configFile () const;
	const kdb::long_t & line () const;

	/* fixed values per Class, taken from C-makro definitions in /src/include/kdberrors.h */
	virtual std::string code () const = 0;

	/* string representation */
	friend std::ostream & operator<< (std::ostream & outputStream, const BaseNotification & eb);

	/**
	 * @brief Compare fields of notification objects
	 *
	 * Also incorporates the compare method to enable subclasses to add constraints to the comparison.
	 *
	 * @param other the notification to compare
	 *
	 * @return true if objects are equal
	 */
	bool operator== (const BaseNotification & other) const;
	bool operator!= (const BaseNotification & other) const;

protected:
	BaseNotification () = default;

	/**
	 * @brief Compare to another notification object
	 *
	 * Is used in operator==.
	 * Can be overloaded by subclasses to check additional constraints.
	 * At least the types of the two objects that get compared should be checked for equality!
	 *
	 * @param other the notification to compare to
	 *
	 * @return true if objects are equal
	 */
	virtual bool compare (const BaseNotification & other) const;

	/* Can be overwritten by subclasses to change the text representation */

	/**
	 * @brief Get a text representation of the notification.
	 *
	 * Is used in operator<<.
	 * Can be overloaded by subclasses to append additional text.
	 *
	 * @param outputStream The stream to append the text to,
	 * used by the operator `<<`
	 *
	 * @return The given stream with additional text appended.
	 */
	virtual std::ostream & toString (std::ostream & outputStream) const;

private:
	std::string m_description;
	std::string m_reason;
	std::string m_module;
	std::string m_file;
	std::string m_mountPoint;
	std::string m_configFile;
	kdb::long_t m_line = 0;
};

} // namespace errors
} // namespace tools
} // namespace kdb
#endif // ELEKTRA_BASENOTIFICATION_HPP

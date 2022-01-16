#ifndef ELEKTRA_BASENOTIFICATION_HPP
#define ELEKTRA_BASENOTIFICATION_HPP

#include <string>
#include <key.hpp>
#include <utility>
#include <kdberrors.h> // for kdb-types, code and description constants

namespace kdb
{
namespace tools
{
namespace errors
{
/* common abstract class for warnings and errors */
/* Because warning and errors share the same data members, a method can accept a ErrBase argument and the caller
 * can create an Error or a Warning based on the provided object. */
class BaseNotification
{
public:
	/* setters */
	void setData (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line);

	/* get references (for getting and setting member values) */
	std::string & reason();
	std::string & module();
	std::string & file();
	kdb::long_t & line();

	/* fixed values per Class, taken from C-makro definitions in /src/include/kdberrors.h */
	virtual std::string code() const = 0;
	virtual std::string description() const = 0;

	/* string representation */
	friend std::ostream& operator<< (std::ostream& outputStream, const BaseNotification& eb);
	/* compare */
	friend bool operator== (const BaseNotification& lhs, const BaseNotification& rhs);

protected:
	BaseNotification () = default;
	BaseNotification (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line);

	/* Can be overwritten by subclasses to change the text representation */
	std::ostream& toString (std::ostream& outputStream) const;

private:
	std::string m_reason;
	std::string m_module;
	std::string m_file;
	kdb::long_t m_line = 0;
};

} // namespace errors
} // namespace tools
} // namespace kdb
#endif // ELEKTRA_BASENOTIFICATION_HPP
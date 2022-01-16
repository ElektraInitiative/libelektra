
#include <kdbprivate.h>
#include <iostream>
#include "errors/baseNotification.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

BaseNotification::BaseNotification (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
{
	setData (reason, module, file, line);
}

void BaseNotification::setData (const std::string & reason, const std::string & module, const std::string & file, kdb::long_t line)
{
	this->m_reason = reason;
	this->m_module = module;
	this->m_file = file;
	this->m_line = line;
}

/* String representation */
std::ostream& BaseNotification::toString (std::ostream & outputStream) const
{
	return outputStream << "Code: " << code()
	        << "\nDescription: " << description()
		<< "\nReason: " << m_reason
		<< "\nModule: " << m_module
		<< "\nFile: " << m_file
		<< "\nLine: " << std::to_string (m_line);
}

std::ostream& operator <<(std::ostream& outputStream, const BaseNotification& eb)
{
	eb.toString (outputStream);
	return outputStream;
}

/* Comparison */
bool BaseNotification::compare(const BaseNotification& other ELEKTRA_UNUSED) const
{
	/* Can be overloaded by subclasses to check additional constraints.
	 * At least the types of the two objects that get compared should be checked for equality! */
	return true;
}

bool BaseNotification::operator== (const BaseNotification& other) const
{
	return code() == other.code()
	       && description() == other.description()
	       && reason() == other.reason()
	       && module() == other.module()
	       && file() == other.file()
	       && line() == other.line()
	       && this->compare(other);
}

bool BaseNotification::operator!= (const BaseNotification& other) const
{
	return !(*this == other);
}

/* setters */
std::string& BaseNotification::reason() { return m_reason; }
std::string& BaseNotification::module() { return m_module; }
std::string& BaseNotification::file() { return m_file; }
kdb::long_t& BaseNotification::line() { return m_line; }

/* getters */
const std::string & BaseNotification::reason () const { return m_reason; }
const std::string & BaseNotification::module () const { return m_module; }
const std::string & BaseNotification::file () const { return m_file; }
const kdb::long_t & BaseNotification::line () const { return m_line; }

} // namespace errors
} // namespace tools
} // namespace kdb

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

bool operator== (const BaseNotification& lhs, const BaseNotification& rhs)
{
	return lhs.code() == rhs.code()
	       && lhs.description() == rhs.description()
	       && lhs.m_reason == rhs.m_reason
	       && lhs.m_module == rhs.m_module
	       && lhs.m_file == rhs.m_file
	       && lhs.m_line == rhs.m_line;
}

/* getters */
std::string& BaseNotification::reason() { return m_reason; }
std::string& BaseNotification::module() { return m_module; }
std::string& BaseNotification::file() { return m_file; }
kdb::long_t& BaseNotification::line() { return m_line; }


} // namespace errors
} // namespace tools
} // namespace kdb
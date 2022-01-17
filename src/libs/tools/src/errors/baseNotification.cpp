
#include <kdbprivate.h>
#include <iostream>
#include <utility>
#include "errors/baseNotification.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{

BaseNotification::BaseNotification (std::string  reason, std::string  module, std::string file,
				    std::string mountPoint, std::string configFile, kdb::long_t line)
: m_reason (std::move (reason)), m_module (std::move (module)), m_file (std::move (file)), m_mountPoint (std::move (mountPoint)),
  m_configFile (std::move (configFile)), m_line (line) {}

void BaseNotification::setData (const std::string & reason, const std::string & module, const std::string & file,
				const std::string & mountPoint, const std::string & configFile, kdb::long_t line)
{
	this->m_reason = reason;
	this->m_module = module;
	this->m_file = file;
	this->m_mountPoint = mountPoint;
	this->m_configFile = configFile;
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
		<< "\nMount point: " << m_mountPoint
		<< "\nConfig file: " << m_configFile
		<< "\nLine: " << std::to_string (m_line);
}

std::ostream& operator <<(std::ostream& outputStream, const BaseNotification& eb)
{
	eb.toString (outputStream);
	return outputStream;
}

bool BaseNotification::compare(const BaseNotification& other ELEKTRA_UNUSED) const
{
	return true;
}

bool BaseNotification::operator== (const BaseNotification& other) const
{
	return code() == other.code()
	       && description() == other.description()
	       && reason() == other.reason()
	       && module() == other.module()
	       && file() == other.file()
	       && mountPoint() == other.mountPoint()
	       && configFile() == other.configFile()
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
std::string& BaseNotification::mountPoint() { return m_mountPoint; }
std::string& BaseNotification::configFile() { return m_configFile; }
kdb::long_t& BaseNotification::line() { return m_line; }

/* getters */
const std::string & BaseNotification::reason () const { return m_reason; }
const std::string & BaseNotification::module () const { return m_module; }
const std::string & BaseNotification::file () const { return m_file; }
const std::string & BaseNotification::mountPoint () const { return m_mountPoint; }
const std::string & BaseNotification::configFile () const { return m_configFile; }
const kdb::long_t & BaseNotification::line () const { return m_line; }

} // namespace errors
} // namespace tools
} // namespace kdb
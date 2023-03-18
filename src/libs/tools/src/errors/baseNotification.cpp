
#include "errors/baseNotification.hpp"
#include <internal/kdbprivate.h>
#include <iostream>
#include <utility>

namespace kdb
{
namespace tools
{
namespace errors
{

BaseNotification::BaseNotification (std::string description, std::string reason, std::string module, std::string file,
				    std::string mountPoint, std::string configFile, kdb::long_t line)
: m_description (std::move (description)), m_reason (std::move (reason)), m_module (std::move (module)), m_file (std::move (file)),
  m_mountPoint (std::move (mountPoint)), m_configFile (std::move (configFile)), m_line (line)
{
}

void BaseNotification::setData (const std::string & description, const std::string & reason, const std::string & module,
				const std::string & file, const std::string & mountPoint, const std::string & configFile, kdb::long_t line)
{
	this->m_description = description;
	this->m_reason = reason;
	this->m_module = module;
	this->m_file = file;
	this->m_mountPoint = mountPoint;
	this->m_configFile = configFile;
	this->m_line = line;
}

/* String representation */
std::ostream & BaseNotification::toString (std::ostream & outputStream) const
{
	return outputStream << "Code: " << code () << std::endl
			    << "Description: " << m_description << std::endl
			    << "Reason: " << m_reason << std::endl
			    << "Module: " << m_module << std::endl
			    << "File: " << m_file << std::endl
			    << "Mount point: " << m_mountPoint << std::endl
			    << "Config file: " << m_configFile << std::endl
			    << "Line: " << std::to_string (m_line);
}

std::ostream & operator<< (std::ostream & outputStream, const BaseNotification & eb)
{
	eb.toString (outputStream);
	return outputStream;
}

bool BaseNotification::compare (const BaseNotification & other ELEKTRA_UNUSED) const
{
	return true;
}

bool BaseNotification::operator== (const BaseNotification & other) const
{
	return code () == other.code () && description () == other.description () && reason () == other.reason () &&
	       module () == other.module () && file () == other.file () && mountPoint () == other.mountPoint () &&
	       configFile () == other.configFile () && line () == other.line () && this->compare (other);
}

bool BaseNotification::operator!= (const BaseNotification & other) const
{
	return !(*this == other);
}

/* setters */
std::string & BaseNotification::reason ()
{
	return m_reason;
}
std::string & BaseNotification::module ()
{
	return m_module;
}
std::string & BaseNotification::file ()
{
	return m_file;
}
std::string & BaseNotification::mountPoint ()
{
	return m_mountPoint;
}
std::string & BaseNotification::configFile ()
{
	return m_configFile;
}
kdb::long_t & BaseNotification::line ()
{
	return m_line;
}

/* getters */
const std::string & BaseNotification::description () const
{
	return m_description;
}
const std::string & BaseNotification::reason () const
{
	return m_reason;
}
const std::string & BaseNotification::module () const
{
	return m_module;
}
const std::string & BaseNotification::file () const
{
	return m_file;
}
const std::string & BaseNotification::mountPoint () const
{
	return m_mountPoint;
}
const std::string & BaseNotification::configFile () const
{
	return m_configFile;
}
const kdb::long_t & BaseNotification::line () const
{
	return m_line;
}

} // namespace errors
} // namespace tools
} // namespace kdb

#include <elektra/kdb/errors.h> // for code and description constants
#include <errors/warningTypes.hpp>

namespace kdb
{
namespace tools
{
namespace errors
{

std::string ResourceWarning::code () const
{
	return ELEKTRA_WARNING_RESOURCE;
}
bool ResourceWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ResourceWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
ResourceWarning * ResourceWarning::clone () const
{
	return new ResourceWarning (*this);
}

std::string OutOfMemoryWarning::code () const
{
	return ELEKTRA_WARNING_OUT_OF_MEMORY;
}
bool OutOfMemoryWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const OutOfMemoryWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
OutOfMemoryWarning * OutOfMemoryWarning::clone () const
{
	return new OutOfMemoryWarning (*this);
}


std::string InstallationWarning::code () const
{
	return ELEKTRA_WARNING_INSTALLATION;
}
bool InstallationWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const InstallationWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
InstallationWarning * InstallationWarning::clone () const
{
	return new InstallationWarning (*this);
}


std::string InternalWarning::code () const
{
	return ELEKTRA_WARNING_INTERNAL;
}
bool InternalWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const InternalWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
InternalWarning * InternalWarning::clone () const
{
	return new InternalWarning (*this);
}


std::string InterfaceWarning::code () const
{
	return ELEKTRA_WARNING_INTERFACE;
}
bool InterfaceWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const InterfaceWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
InterfaceWarning * InterfaceWarning::clone () const
{
	return new InterfaceWarning (*this);
}


std::string PluginMisbehaviorWarning::code () const
{
	return ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR;
}
bool PluginMisbehaviorWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const PluginMisbehaviorWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
PluginMisbehaviorWarning * PluginMisbehaviorWarning::clone () const
{
	return new PluginMisbehaviorWarning (*this);
}

std::string ConflictingStateWarning::code () const
{
	return ELEKTRA_WARNING_CONFLICTING_STATE;
}
bool ConflictingStateWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ConflictingStateWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
ConflictingStateWarning * ConflictingStateWarning::clone () const
{
	return new ConflictingStateWarning (*this);
}


std::string ValidationSyntacticWarning::code () const
{
	return ELEKTRA_WARNING_VALIDATION_SYNTACTIC;
}
bool ValidationSyntacticWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ValidationSyntacticWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
ValidationSyntacticWarning * ValidationSyntacticWarning::clone () const
{
	return new ValidationSyntacticWarning (*this);
}


std::string ValidationSemanticWarning::code () const
{
	return ELEKTRA_WARNING_VALIDATION_SEMANTIC;
}
bool ValidationSemanticWarning::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ValidationSemanticWarning *> (&other)))
		return false;
	else
		return Warning::compare (other);
}
ValidationSemanticWarning * ValidationSemanticWarning::clone () const
{
	return new ValidationSemanticWarning (*this);
}


} // namespace errors
} // namespace tools
} // namespace kdb

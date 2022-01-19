
#include <errors/errorTypes.hpp>
#include <kdberrors.h> // for code and description constants

namespace kdb
{
namespace tools
{
namespace errors
{

std::string PureWarningError::code () const
{
	return "";
}
std::string PureWarningError::description () const
{
	return "Warnings";
}
bool PureWarningError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const PureWarningError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string ResourceError::code () const
{
	return ELEKTRA_ERROR_RESOURCE;
}
std::string ResourceError::description () const
{
	return ELEKTRA_ERROR_RESOURCE_NAME;
}
bool ResourceError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ResourceError *> (&other)))
		return false;
	else
		return Error::compare (other);
}


std::string OutOfMemoryError::code () const
{
	return ELEKTRA_ERROR_OUT_OF_MEMORY;
}
std::string OutOfMemoryError::description () const
{
	return ELEKTRA_ERROR_OUT_OF_MEMORY_NAME;
}
bool OutOfMemoryError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const OutOfMemoryError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string InstallationError::code () const
{
	return ELEKTRA_ERROR_INSTALLATION;
}
std::string InstallationError::description () const
{
	return ELEKTRA_ERROR_INSTALLATION_NAME;
}
bool InstallationError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const InstallationError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string InternalError::code () const
{
	return ELEKTRA_ERROR_INTERNAL;
}
std::string InternalError::description () const
{
	return ELEKTRA_ERROR_INTERNAL_NAME;
}
bool InternalError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const InternalError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string InterfaceError::code () const
{
	return ELEKTRA_ERROR_INTERFACE;
}
std::string InterfaceError::description () const
{
	return ELEKTRA_ERROR_INTERFACE_NAME;
}
bool InterfaceError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const InterfaceError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string PluginMisbehaviorError::code () const
{
	return ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR;
}
std::string PluginMisbehaviorError::description () const
{
	return ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR_NAME;
}
bool PluginMisbehaviorError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const PluginMisbehaviorError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string ConflictingStateError::code () const
{
	return ELEKTRA_ERROR_CONFLICTING_STATE;
}
std::string ConflictingStateError::description () const
{
	return ELEKTRA_ERROR_CONFLICTING_STATE_NAME;
}
bool ConflictingStateError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ConflictingStateError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string ValidationSyntacticError::code () const
{
	return ELEKTRA_ERROR_VALIDATION_SYNTACTIC;
}
std::string ValidationSyntacticError::description () const
{
	return ELEKTRA_ERROR_VALIDATION_SYNTACTIC_NAME;
}
bool ValidationSyntacticError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ValidationSyntacticError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string ValidationSemanticError::code () const
{
	return ELEKTRA_ERROR_VALIDATION_SEMANTIC;
}
std::string ValidationSemanticError::description () const
{
	return ELEKTRA_ERROR_VALIDATION_SEMANTIC_NAME;
}
bool ValidationSemanticError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ValidationSemanticError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

} // namespace errors
} // namespace tools
} // namespace kdb

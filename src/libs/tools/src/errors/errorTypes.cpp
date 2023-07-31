
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
bool ValidationSemanticError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const ValidationSemanticError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

std::string CliError::code () const
{
	return ELEKTRA_ERROR_CLI;
}
bool CliError::compare (const BaseNotification & other) const
{
	if (!(dynamic_cast<const CliError *> (&other)))
		return false;
	else
		return Error::compare (other);
}

} // namespace errors
} // namespace tools
} // namespace kdb


#include "errors/warningFactory.hpp"
#include "errors/warningTypes.hpp"
#include <kdberrors.h> // for code and description constants

namespace kdb
{
namespace tools
{
namespace errors
{

Warning * WarningFactory::create (const std::string & type, const std::string & reason, const std::string & module,
				  const std::string & file, const std::string & mountPoint, const std::string & configFile,
				  kdb::long_t line)
{
	if (type == ELEKTRA_WARNING_RESOURCE || type == ELEKTRA_WARNING_RESOURCE_NAME)
		return new ResourceWarning (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_OUT_OF_MEMORY || type == ELEKTRA_WARNING_OUT_OF_MEMORY_NAME)
		return new OutOfMemoryWarning (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_INSTALLATION || type == ELEKTRA_WARNING_INSTALLATION_NAME)
		return new InstallationWarning (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_INTERNAL || type == ELEKTRA_WARNING_INTERNAL_NAME)
		return new InternalWarning (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_INTERFACE || type == ELEKTRA_WARNING_INTERFACE_NAME)
		return new InterfaceWarning (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR || type == ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR_NAME)
		return new PluginMisbehaviorWarning (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_CONFLICTING_STATE || type == ELEKTRA_WARNING_CONFLICTING_STATE_NAME)
		return new ConflictingStateWarning (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_VALIDATION_SYNTACTIC || type == ELEKTRA_WARNING_VALIDATION_SYNTACTIC_NAME)
		return new ValidationSyntacticWarning (reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_VALIDATION_SEMANTIC || type == ELEKTRA_WARNING_VALIDATION_SEMANTIC_NAME)
		return new ValidationSemanticWarning (reason, module, file, mountPoint, configFile, line);
	else
		return nullptr;
}

bool WarningFactory::checkWarningCodeDesc (const std::string & code, const std::string & description)
{
	if (code == ELEKTRA_WARNING_RESOURCE)
		return (description == ELEKTRA_WARNING_RESOURCE_NAME);
	else if (code == ELEKTRA_WARNING_OUT_OF_MEMORY)
		return (description == ELEKTRA_WARNING_OUT_OF_MEMORY_NAME);
	else if (code == ELEKTRA_WARNING_INSTALLATION)
		return (description == ELEKTRA_WARNING_INSTALLATION_NAME);
	else if (code == ELEKTRA_WARNING_INTERNAL)
		return (description == ELEKTRA_WARNING_INTERNAL_NAME);
	else if (code == ELEKTRA_WARNING_INTERFACE)
		return (description == ELEKTRA_WARNING_INTERFACE_NAME);
	else if (code == ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR)
		return (description == ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR_NAME);
	else if (code == ELEKTRA_WARNING_CONFLICTING_STATE)
		return (description == ELEKTRA_WARNING_CONFLICTING_STATE_NAME);
	else if (code == ELEKTRA_WARNING_VALIDATION_SYNTACTIC)
		return (description == ELEKTRA_WARNING_VALIDATION_SYNTACTIC_NAME);
	else if (code == ELEKTRA_WARNING_VALIDATION_SEMANTIC)
		return (description == ELEKTRA_WARNING_VALIDATION_SEMANTIC_NAME);
	else
		return false;
}

} // namespace errors
} // namespace tools
} // namespace kdb

#include "errors/warningFactory.hpp"
#include "errors/warningTypes.hpp"
#include <kdberrors.h> // for code and description constants

namespace kdb
{
namespace tools
{
namespace errors
{

Warning * WarningFactory::create (const std::string & type, const std::string & description, const std::string & reason,
				  const std::string & module, const std::string & file, const std::string & mountPoint,
				  const std::string & configFile, kdb::long_t line)
{
	if (type == ELEKTRA_WARNING_RESOURCE)
		return new ResourceWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_OUT_OF_MEMORY)
		return new OutOfMemoryWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_INSTALLATION)
		return new InstallationWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_INTERNAL)
		return new InternalWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_INTERFACE)
		return new InterfaceWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR)
		return new PluginMisbehaviorWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_CONFLICTING_STATE)
		return new ConflictingStateWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_VALIDATION_SYNTACTIC)
		return new ValidationSyntacticWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_VALIDATION_SEMANTIC)
		return new ValidationSemanticWarning (description, reason, module, file, mountPoint, configFile, line);
	else if (type == ELEKTRA_WARNING_CLI)
		return new CliWarning (description, reason, module, file, mountPoint, configFile, line);
	else
		return nullptr;
}

} // namespace errors
} // namespace tools
} // namespace kdb
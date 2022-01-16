
#include <errors/errorTypes.hpp>

namespace kdb
{
namespace tools
{
namespace errors
{

std::string ResourceError::code() const { return ELEKTRA_ERROR_RESOURCE; }
std::string ResourceError::description() const { return ELEKTRA_ERROR_RESOURCE_NAME; }

std::string OutOfMemoryError::code() const { return ELEKTRA_ERROR_OUT_OF_MEMORY; }
std::string OutOfMemoryError::description() const { return ELEKTRA_ERROR_OUT_OF_MEMORY_NAME; }

std::string InstallationError::code() const { return ELEKTRA_ERROR_INSTALLATION; }
std::string InstallationError::description() const { return ELEKTRA_ERROR_INSTALLATION_NAME; }

std::string InternalError::code() const { return ELEKTRA_ERROR_INTERNAL; }
std::string InternalError::description() const { return ELEKTRA_ERROR_INTERNAL_NAME; }

std::string InterfaceError::code() const { return ELEKTRA_ERROR_INTERFACE; }
std::string InterfaceError::description() const { return ELEKTRA_ERROR_INTERFACE_NAME; }

std::string PluginMisbehaviorError::code() const { return ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR; }
std::string PluginMisbehaviorError::description() const { return ELEKTRA_ERROR_PLUGIN_MISBEHAVIOR_NAME; }

std::string ConflictingStateError::code() const { return ELEKTRA_ERROR_CONFLICTING_STATE; }
std::string ConflictingStateError::description() const { return ELEKTRA_ERROR_CONFLICTING_STATE_NAME; }

std::string ValidationSyntacticError::code() const { return ELEKTRA_ERROR_VALIDATION_SYNTACTIC; }
std::string ValidationSyntacticError::description() const { return ELEKTRA_ERROR_VALIDATION_SYNTACTIC_NAME; }

std::string ValidationSemanticError::code() const { return ELEKTRA_ERROR_VALIDATION_SEMANTIC; }
std::string ValidationSemanticError::description() const { return ELEKTRA_ERROR_VALIDATION_SEMANTIC_NAME; }

} // namespace errors
} // namespace tools
} // namespace kdb

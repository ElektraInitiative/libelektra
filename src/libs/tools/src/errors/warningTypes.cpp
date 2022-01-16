

#include <errors/warningTypes.hpp>

namespace kdb
{
namespace tools
{
namespace errors
{

std::string ResourceWarning::code() const { return ELEKTRA_WARNING_RESOURCE; }
std::string ResourceWarning::description() const { return ELEKTRA_WARNING_RESOURCE_NAME; }

std::string OutOfMemoryWarning::code() const { return ELEKTRA_WARNING_OUT_OF_MEMORY; }
std::string OutOfMemoryWarning::description() const { return ELEKTRA_WARNING_OUT_OF_MEMORY_NAME; }

std::string InstallationWarning::code() const { return ELEKTRA_WARNING_INSTALLATION; }
std::string InstallationWarning::description() const { return ELEKTRA_WARNING_INSTALLATION_NAME; }

std::string InternalWarning::code() const { return ELEKTRA_WARNING_INTERNAL; }
std::string InternalWarning::description() const { return ELEKTRA_WARNING_INTERNAL_NAME; }

std::string InterfaceWarning::code() const { return ELEKTRA_WARNING_INTERFACE; }
std::string InterfaceWarning::description() const { return ELEKTRA_WARNING_INTERFACE_NAME; }

std::string PluginMisbehaviorWarning::code() const { return ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR; }
std::string PluginMisbehaviorWarning::description() const { return ELEKTRA_WARNING_PLUGIN_MISBEHAVIOR_NAME; }

std::string ConflictingStateWarning::code() const { return ELEKTRA_WARNING_CONFLICTING_STATE; }
std::string ConflictingStateWarning::description() const { return ELEKTRA_WARNING_CONFLICTING_STATE_NAME; }

std::string ValidationSyntacticWarning::code() const { return ELEKTRA_WARNING_VALIDATION_SYNTACTIC; }
std::string ValidationSyntacticWarning::description() const { return ELEKTRA_WARNING_VALIDATION_SYNTACTIC_NAME; }

std::string ValidationSemanticWarning::code() const { return ELEKTRA_WARNING_VALIDATION_SEMANTIC; }
std::string ValidationSemanticWarning::description() const { return ELEKTRA_WARNING_VALIDATION_SEMANTIC_NAME; }

} // namespace errors
} // namespace tools
} // namespace kdb

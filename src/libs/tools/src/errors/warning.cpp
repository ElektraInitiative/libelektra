
#include <kdberrors.h>
#include "errors/warning.hpp"


namespace kdb
{

namespace tools
{
namespace errors
{

void Warning::setSemanticValidationWarning (const std::string & description, const std::string & module, const std::string & file,
					    kdb::long_t line)
{
	setData(ELEKTRA_WARNING_VALIDATION_SEMANTIC, description, module, file, line);
}

void Warning::setSyntacticValidationWarning (const std::string & description, const std::string & module, const std::string & file,
					     kdb::long_t line)
{
	setData(ELEKTRA_WARNING_VALIDATION_SYNTACTIC, description, module, file, line);
}


} // namespace errors
} // namespace tools
} // namespace kdb
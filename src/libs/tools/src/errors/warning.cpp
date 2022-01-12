#include "errors/warning.hpp"
//#include "kdberrors.h"

namespace kdb
{

namespace tools
{

void Warning::setSemanticValidationWarning (const std::string & description, const std::string & module,
					    const std::string & file, /*kdb::long_t*/long line)
{
	//setData(ELEKTRA_WARNING_VALIDATION_SEMANTIC, description, module, file, line);
}

void Warning::setSyntacticValidationWarning (const std::string & description, const std::string & module,
					     const std::string & file, /*kdb::long_t*/long line)
{
	//setData(ELEKTRA_WARNING_VALIDATION_SYNTACTIC, description, module, file, line);
}

} // namespace tools
} // namespace kdb

#ifndef ELEKTRA_WARNING_HPP
#define ELEKTRA_WARNING_HPP

#include "errors/errbase.hpp"

namespace kdb
{


namespace tools
{

class Warning : public ErrBase
{
public:
	Warning (const std::string & code, const std::string & description, const std::string & module,
		 const std::string & file, /*kdb::long_t*/long line)
	: ErrBase {code, description, module, file, line} {}
	~Warning() override = default;
	void setSemanticValidationWarning (const std::string & description, const std::string & module,
					   const std::string & file, /*kdb::long_t*/long line);
	void setSyntacticValidationWarning (const std::string & description, const std::string & module,
					    const std::string & file, /*kdb::long_t*/long line);
};


} // namespace tools
} // namespace kdb
#endif // ELEKTRA_WARNING_HPP

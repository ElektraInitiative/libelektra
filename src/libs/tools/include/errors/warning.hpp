
#ifndef ELEKTRA_WARNING_HPP
#define ELEKTRA_WARNING_HPP

#include "errors/errbase.hpp"

namespace kdb
{


namespace tools
{

namespace errors
{

class Warning : public ErrBase
{
public:
	Warning (const std::string & code, const std::string & description, const std::string & module, const std::string & file,
		 kdb::long_t line)
	: ErrBase { code, description, module, file, line }
	{
	}
	Warning () = default;
	explicit Warning (ElektraError *err) : ErrBase { err } {}
	~Warning () override = default;
	void setSemanticValidationWarning (const std::string & description, const std::string & module, const std::string & file,
					   kdb::long_t line);
	void setSyntacticValidationWarning (const std::string & description, const std::string & module, const std::string & file,
					    kdb::long_t line);
private:
	friend class Error; // Error has access to ElektraError *err (internal errors) of warnings
};

} // namespace errors
} // namespace tools
} // namespace kdb
#endif // ELEKTRA_WARNING_HPP

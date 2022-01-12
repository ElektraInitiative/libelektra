#ifndef ELEKTRA_ERROR_HPP
#define ELEKTRA_ERROR_HPP

#include <errors/warning.hpp>

namespace kdb
{


namespace tools
{


class Error : public ErrBase
{
public:
	Error () : ErrBase {} {}
	Error (const std::string & code, const std::string & description, const std::string & module,
	       const std::string & file, /*kdb::long_t*/long line)
		: ErrBase {code, description, module, file, line} {}
	explicit Error (kdb::Key & errorKey) : ErrBase {errorKey} {}
	~Error () override;

	void addWarning (std::string code, std::string description, std::string module, std::string file, /*kdb::long_t*/long line);
	void addWarning (Warning & warning);
	void addWarning (/*ElektraError*/char* warning);

	/* getters */
	/*kdb::long_t*/long warningCount ();

	/* special types of errors and warnings */
	void setSemanticValidationError (const std::string & description, const std::string & module,
					 const std::string & file, /*kdb::long_t*/long line);
	void setSyntacticValidationError (const std::string & description, const std::string & module,
					  const std::string & file, /*kdb::long_t*/long line);

};

} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERROR_HPP

#ifndef ELEKTRA_ERROR_HPP
#define ELEKTRA_ERROR_HPP

#include <iterator>
#include <cstddef>
#include <errors/warning.hpp>

namespace kdb
{
namespace tools
{

namespace errors
{

class Error : public ErrBase
{
public:
	Error () : ErrBase{}
	{
	}
	Error (const std::string & code, const std::string & description, const std::string & module, const std::string & file,
	       kdb::long_t line)
	: ErrBase{ code, description, module, file, line }
	{
	}
	explicit Error (kdb::Key & errorKey) : ErrBase{ errorKey }
	{
	}
	explicit Error (ElektraError *err) : ErrBase { err } {}
	~Error () override;

	void addWarning (const std::string& code, const std::string& description, const std::string& module, const std::string& file,
			 kdb::long_t line);
	void addWarning (Warning & warning);
	void addWarning (ElektraError * warning);

	/* getters */
	kdb::long_t warningCount ();
	Warning getWarning(kdb::long_t index);
	Warning copyWarning(kdb::long_t index);


	/* special types of errors and warnings */
	void setSemanticValidationError (const std::string & description, const std::string & module, const std::string & file,
					 kdb::long_t line);
	void setSyntacticValidationError (const std::string & description, const std::string & module, const std::string & file,
					  kdb::long_t line);


};
} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERROR_HPP

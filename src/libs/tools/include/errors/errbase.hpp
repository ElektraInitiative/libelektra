#ifndef ELEKTRA_ERRBASE_HPP
#define ELEKTRA_ERRBASE_HPP

#include <string>
#include <key.hpp>
//#include "elektra/error.h"

namespace kdb
{

namespace tools
{

/* common abstract class for warnings and errors */
class ErrBase
{
public:
	ErrBase () = default;
	ErrBase (const std::string & code, const std::string & description, const std::string & module,
		 const std::string & file, /*kdb::long_t line*/ long line);
	explicit ErrBase (kdb::Key & errKey);
	virtual ~ErrBase () = default; // pure virtual destructor

	/* setters */
	void setData (kdb::Key & errKey);
	void setData (const std::string & code, const std::string & description, const std::string & module, const std::string & file, /*kdb::long_t*/ long line);

	/* getters */
	std::string errorCode ();
	std::string description ();
	std::string module ();
	std::string file ();
	long line (); //kdb::long_t line ();
	/*ElektraError*/char* internalError ();

protected:
	/*ElektraError*/char* err = nullptr;
};


} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERRBASE_HPP

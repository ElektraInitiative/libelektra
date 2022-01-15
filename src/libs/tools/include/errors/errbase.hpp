#ifndef ELEKTRA_ERRBASE_HPP
#define ELEKTRA_ERRBASE_HPP

#include <string>
#include <key.hpp>
#include <elektra.h>

namespace kdb
{

namespace tools
{

namespace errors
{
	/* common abstract class for warnings and errors */

class ErrBase
{
public:
	ErrBase () = default;
	ErrBase (const std::string & code, const std::string & description, const std::string & module, const std::string & file,
		 kdb::long_t line);
	explicit ErrBase (kdb::Key & errKey);
	explicit ErrBase (ElektraError *err);
	virtual ~ErrBase () = default; // pure virtual destructor

	/* setters */
	void setData (kdb::Key & errKey);
	void setData (ElektraError * err);
	void setData (const std::string & code, const std::string & description, const std::string & module, const std::string & file,
		      kdb::long_t line);

	/* getters */
	std::string errorCode ();
	std::string description ();
	std::string module ();
	std::string file ();
	kdb::long_t line ();
	kdb::boolean_t isNull();

protected:
	ElektraError * err = nullptr;
};

} // namespace errors
} // namespace tools
} // namespace kdb

#endif // ELEKTRA_ERRBASE_HPP
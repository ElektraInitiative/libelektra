
#include <kdbprivate.h>
#include "errors/errbase.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{
ErrBase::ErrBase (const std::string & code, const std::string & description, const std::string & module, const std::string & file,
		  kdb::long_t line)
{
	setData (code, description, module, file, line);
}

ErrBase::ErrBase (kdb::Key & errKey)
{
	setData (errKey);
}

ErrBase::ErrBase (ElektraError * err)
{
	this->err = err;
}


void ErrBase::setData (ElektraError * _err)
{
	this->err = _err;
}

void ErrBase::setData (kdb::Key & errKey)
{
	if (err)
	{
		elektraErrorReset (&err);
	}
	err = elektraErrorFromKey (errKey.getKey ());
}


void ErrBase::setData (const std::string & code, const std::string & description, const std::string & module, const std::string & file,
		       kdb::long_t line)
{
	if (err)
	{
		elektraErrorReset (&err);
	}
	// strings get duplicated by elektraErrorCreate
	err = elektraErrorCreate (code.c_str (), description.c_str (), module.c_str (), file.c_str (), line);
}

/* getters */
std::string ErrBase::errorCode ()
{
	elektraErrorReset (&err);
	return elektraErrorCode (err);
}

std::string ErrBase::description ()
{
	return elektraErrorDescription (err);
}

/* TODO: maybe implement getter functions in C (currently in highlevel-API (elektra_error.c))
 * and call them like for code and description */
std::string ErrBase::module ()
{
	return err->module;
}

std::string ErrBase::file ()
{
	return err->file;
}

kdb::long_t ErrBase::line ()
{
	return err->line;
}

kdb::boolean_t ErrBase::isNull ()
{
	return (err == nullptr);
}

} // namespace errors
} // namespace tools
} // namespace kdb

#include "errors/errbase.hpp"
//#include "kdbprivate.h"

namespace kdb
{

namespace tools
{

ErrBase::ErrBase (const std::string & code, const std::string & description, const std::string & module,
		  const std::string & file, /*kdb::long_t*/ long line)
{
	setData(code, description, module, file, line);
}

ErrBase::ErrBase (kdb::Key & errKey)
{
	setData (errKey);
}

void ErrBase::setData (kdb::Key & errKey)
{
	if (err)
	{
		err = nullptr;
		//elektraErrorReset (&err);
	}
	err = nullptr; //elektraErrorFromKey (errKey.getKey());
}


void ErrBase::setData (const std::string & code, const std::string & description, const std::string & module, const std::string & file, /*kdb::long_t*/ long line)
{
	if (err)
	{
		err = nullptr;
		//elektraErrorReset (&err);
	}
	// strings get duplicated by elektraErrorCreate
	err = nullptr; //elektraErrorCreate (code.c_str(), description.c_str(), module.c_str(), file.c_str(), line);
}

/* getters */
std::string ErrBase::errorCode ()
{
	return ""; //elektraErrorCode(err);
}

std::string ErrBase::description ()
{
	return ""; //elektraErrorDescription(err);
}

/* TODO: maybe implement functions C (currently in highlevel-API (elektra_error.c)
 * and call them like for code and description */
std::string ErrBase::module ()
{
	return ""; //err->module;
}

std::string ErrBase::file ()
{
	return ""; //err->file;
}

/*kdb::long_t*/ long ErrBase::line ()
{
	return 0; //err->line;
}

/*ElektraError*/char* ErrBase::internalError () {
	return nullptr; //err;
}

} // namespace tools
} // namespace kdb
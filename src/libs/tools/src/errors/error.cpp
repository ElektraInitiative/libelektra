

#include <kdbprivate.h>
#include <kdberrors.h>
#include "errors/error.hpp"

namespace kdb
{
namespace tools
{
namespace errors
{


Error::~Error ()
{
	if (err)
	{
		elektraErrorReset (&err);
	}
}


void Error::addWarning (const std::string & code, const std::string & description, const std::string & module,
			const std::string & file, kdb::long_t line)
{
	Warning w { code, description, module, file, line };
	addWarning (w);
}

void Error::addWarning (ElektraError * warning)
{
	if (!err)
	{
		// create a dummy error key for only storing warnings
		setData (elektraErrorPureWarning ());
	}
	elektraErrorAddWarning (err, warning);
}

void Error::addWarning (Warning & warning)
{
	addWarning (warning.err);
}

/* getters */
kdb::long_t Error::warningCount ()
{
	return err->warningCount;
}


void Error::setSemanticValidationError (const std::string & description, const std::string & module, const std::string & file,
					kdb::long_t line)
{
	setData (ELEKTRA_ERROR_VALIDATION_SEMANTIC, description, module, file, line);
}

void Error::setSyntacticValidationError (const std::string & description, const std::string & module, const std::string & file,
					 kdb::long_t line)
{
	setData (ELEKTRA_ERROR_VALIDATION_SYNTACTIC, description, module, file, line);
}

/* Extract Warning from Error object
 * Caution: No deep copy of the underlying ElektraError* err is created!
 * The memory for it must be freed by the destructor of this Error object!
 * The returned Warning must not be used after the Error object it originated from was destructed!
 * This method can be used for conveniently iterating over the Warnings of an Error */
Warning Error::getWarning(kdb::long_t index)
{
	return (err->warningCount>index) ? (Warning (err->warnings[index])) : Warning ();
}

/* Copy a Warning from an Error Object, a deep copy of the underlying ElektraError *err is made.
 * Can be used for adding it to another Error object, the memory for the Warning
 * than gets freed by the Destructor of the other Error. */
Warning Error::copyWarning(kdb::long_t index)
{
	if (err->warningCount > index)
	{
		return { err->code, err->description, err->module, err->file, err->line };
	}
	else
	{
		return {};
	}
}


} // namespace errors
} // namespace tools
} // namespace kdb
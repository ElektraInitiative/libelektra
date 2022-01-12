

//#include "kdbprivate.h"
//#include "kdberrors.h"
#include <errors/error.hpp>

namespace kdb
{
namespace tools
{



Error::~Error ()
{
	if (err)
	{
		err = nullptr;
		//elektraErrorReset (&err);
	}
}


void Error::addWarning (std::string code, std::string description, std::string module, std::string file, /*kdb::long_t*/long line)
{
	Warning w {std::move(code), std::move(description), std::move(module), std::move(file), line};
	addWarning (w);
}

void Error::addWarning (/*ElektraError*/char* warning)
{
	if(!err) {
		// create a dummy error key for only storing warnings
		//err = ;//elektraErrorPureWarning();
	}
	//elektraErrorAddWarning (err, warning);
}

void Error::addWarning (Warning & warning)
{
	addWarning (warning.internalError());
}

/* getters */
/*kdb::long_t*/long Error::warningCount ()
{
	return 0; //err->warningCount;
}



void Error::setSemanticValidationError (const std::string & description, const std::string & module,
					const std::string & file, /*kdb::long_t*/long line)
{
	//setData(ELEKTRA_ERROR_VALIDATION_SEMANTIC, description, module, file, line);
}

void Error::setSyntacticValidationError (const std::string & description, const std::string & module,
					 const std::string & file, /*kdb::long_t*/long line)
{
	//setData(ELEKTRA_ERROR_VALIDATION_SYNTACTIC, description, module, file, line);
}



} // namespace tools
} // namespace kdb
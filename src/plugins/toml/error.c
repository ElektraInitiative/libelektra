#include "error.h"

#include <kdb.h>
#include <kdbassert.h>
#include <kdberrors.h>

#include "driver.h"

extern int yylineno;

static void emitElektraError (Key * root, int err, const char * msg);

int yyerror (Driver * driver, const char * msg)
{
	driverError (driver, ERROR_SYNTACTIC, yylineno, msg);
	return 0;
}

void driverError (Driver * driver, int err, int lineno, const char * format, ...)
{
	va_list args;
	char msg[256];
	va_start (args, format);
	if (lineno > 0)
	{
		snprintf (msg, 256, "Line ~%d: ", lineno);
		size_t len = strlen (msg);
		vsnprintf (msg + len, 256 - len, format, args);
	}
	else
	{
		vsnprintf (msg, 256, format, args);
	}
	va_end (args);
	if (!driver->errorSet)
	{
		driver->errorSet = true;
		emitElektraError (driver->root, err, msg);
		ELEKTRA_LOG_DEBUG ("Error: %s", msg);
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("Additional Error: %s", msg);
	}
}

void driverErrorGeneric (Driver * driver, int err, const char * caller, const char * callee)
{
	if (!driver->errorSet)
	{
		char msg[256];
		driver->errorSet = true;
		snprintf (msg, 256, "%s: Error during call of %s", caller, callee);
		emitElektraError (driver->root, err, msg);
		ELEKTRA_LOG_DEBUG ("Error: %s\n", msg);
	}
}

static void emitElektraError (Key * root, int err, const char * msg)
{
	switch (err)
	{
	case ERROR_INTERNAL:
		ELEKTRA_SET_INTERNAL_ERROR (root, msg);
		break;
	case ERROR_MEMORY:
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (root, msg);
		break;
	case ERROR_SYNTACTIC:
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (root, msg);
		break;
	case ERROR_SEMANTIC:
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (root, msg);
		break;
	default:
		ELEKTRA_SET_INTERNAL_ERROR (root, msg);
		break;
	}
}

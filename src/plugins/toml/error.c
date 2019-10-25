#include "error.h"

#include "driver.h"
#include "error_code.h"

extern int yylineno;

int yyerror (Driver * driver, const char * msg)
{
	driverError (driver, ERROR_SYNTACTIC, yylineno, msg);
	return 0;
}

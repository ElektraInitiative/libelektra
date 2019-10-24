#include "error.h"

#include "driver.h"

extern int yylineno;

int yyerror (Driver * driver, const char * msg)
{
	driverError (driver, yylineno, msg);
	return 0;
}

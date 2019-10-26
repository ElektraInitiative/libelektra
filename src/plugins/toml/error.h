#ifndef ELEKTRA_PLUGIN_TOML_ERROR_H
#define ELEKTRA_PLUGIN_TOML_ERROR_H

#include <stdio.h>
#include <stdlib.h>

#include "driver.h"

#define ERROR_MEMORY 0xC01110
#define ERROR_INTERNAL 0xC01310
#define ERROR_SYNTACTIC 0xC03100
#define ERROR_SEMANTIC 0xC03200

int yyerror (Driver * driver, const char * msg);
void driverError (Driver * driver, int err, int lineno, const char * format, ...);
void driverErrorGeneric (Driver * driver, int err, const char * caller, const char * callee);

#endif // ELEKTRA_PLUGIN_TOML_ERROR_H

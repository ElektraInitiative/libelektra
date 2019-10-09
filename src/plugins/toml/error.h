#ifndef ELEKTRA_PLUGIN_TOML_ERROR_H
#define ELEKTRA_PLUGIN_TOML_ERROR_H

#include <stdio.h>
#include <stdlib.h>

#include "driver.h"

int yyerror (Driver * driver, const char * msg);

#endif // ELEKTRA_PLUGIN_TOML_ERROR_H

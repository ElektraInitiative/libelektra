/**
 * @file integer.h
 *
 * @brief Contains functionality for validation/conversion of integers
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_TOML_INTEGER_H
#define ELEKTRA_PLUGIN_TOML_INTEGER_H

#include <stdbool.h>

bool isValidIntegerAnyBase (const char * str);
bool isValidInteger (const char * str, unsigned long long base);


#endif // ELEKTRA_PLUGIN_TOML_INTEGER_H

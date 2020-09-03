/**
 * @file error.h
 *
 * @brief Contains functionality for handling errors.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef ELEKTRA_PLUGIN_TOML_ERROR_H
#define ELEKTRA_PLUGIN_TOML_ERROR_H

#include <kdb.h>

#define ERROR_MEMORY 0xC01110
#define ERROR_INTERNAL 0xC01310
#define ERROR_SYNTACTIC 0xC03100
#define ERROR_SEMANTIC 0xC03200

/*
 * @brief Write a proper elektra error to the given key.
 *
 * @param root Key on which to write metakeys, usually the file root key.
 * @param err Internal error number to identify the error type.
 * @param msg Error message to be written to the key.
 */
void emitElektraError (Key * root, int err, const char * msg);

#endif // ELEKTRA_PLUGIN_TOML_ERROR_H

/*
 * @file write.h
 *
 * @brief Contains functionality for writing a TOML file from an Elektra keyset
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_TOML_WRITE_H
#define ELEKTRA_PLUGIN_TOML_WRITE_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <stdlib.h>

/*
 * @brief Writes the given keyset to a TOML file.
 *
 * @param keys Keyset to be written
 * @param parent File root key, must contain filename to be written.
 *
 * @retval 0 On success.
 * @retval Non-Zero On Error, Error information written to parent key.
 */
int tomlWrite (KeySet * keys, Key * parent);

#endif // ELEKTRA_PLUGIN_TOML_WRITE_H

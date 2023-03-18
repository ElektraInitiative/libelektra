/**
 * @file prepare.h
 *
 * @brief Contains functionality for preparing a keyset to be written.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_TOML_PREPARE_H
#define ELEKTRA_PLUGIN_TOML_PREPARE_H

#include <elektra/kdb.h>
#include <stdbool.h>

/* @brief Prepares a keyset for writing.
 *
 * Adds missing array metakeys, removes invalid array metakeys, adds missing order metakeys and adds missing comment metakeys.
 *
 * @param keys Keyset which should be prepared.
 * @param parent The parent key of the keyset.
 *
 * @retval true On success
 * @retval false If unordered keys could not be prepared.
 */
bool prepareKeySet (KeySet * keys, Key * parent);

#endif // ELEKTRA_PLUGIN_TOML_PREPARE_H

/**
 * @file
 *
 * @brief A plugin which logs write operations and errors via the native journald interface
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_JOURNALD_H
#define ELEKTRA_PLUGIN_JOURNALD_H

#include <elektra/plugin/plugin.h>

int elektraJournaldGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraJournaldCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraJournaldError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

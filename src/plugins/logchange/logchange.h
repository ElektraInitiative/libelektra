/**
 * @file
 *
 * @brief Header for logchange plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LOGCHANGE_H
#define ELEKTRA_PLUGIN_LOGCHANGE_H

#include <elektra/kdbplugin.h>


int elektraLogchangeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLogchangeCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLogchangeClose (Plugin * handle, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

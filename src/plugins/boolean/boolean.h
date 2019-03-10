/**
 * @file
 *
 * @brief Header for boolean plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BOOLEAN_H
#define ELEKTRA_PLUGIN_BOOLEAN_H

#include <kdbplugin.h>


int elektraBooleanClose (Plugin * handle, Key * errorKey);
int elektraBooleanGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBooleanSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

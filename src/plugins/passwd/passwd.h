/**
 * @file
 *
 * @brief Header for passwd plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_PASSWD_H
#define ELEKTRA_PLUGIN_PASSWD_H

#include <elektra/plugin/plugin.h>


int elektraPasswdGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraPasswdSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

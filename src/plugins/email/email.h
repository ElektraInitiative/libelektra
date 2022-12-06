/**
 * @file
 *
 * @brief Header for email plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_EMAIL_H
#define ELEKTRA_PLUGIN_EMAIL_H

#include <elektra/kdbplugin.h>

int elektraEmailGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraEmailSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

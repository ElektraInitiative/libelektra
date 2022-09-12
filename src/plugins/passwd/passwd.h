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

#include <kdbplugin.h>


int elektraPasswdGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraPasswdSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

/**
 * @file
 *
 * @brief Header for passwd plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_PASSWD_H
#define ELEKTRA_PLUGIN_PASSWD_H

#include <kdbplugin.h>


int elektraPasswdGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraPasswdSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (passwd);

#endif

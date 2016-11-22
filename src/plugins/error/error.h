/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <kdberrors.h>
#include <kdbplugin.h>


int elektraErrorGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraErrorSet (Plugin * handle, KeySet * ks, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT (error);

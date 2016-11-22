/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_SYNC_H
#define ELEKTRA_PLUGIN_SYNC_H

#include <kdbplugin.h>


int elektraSyncOpen (Plugin * handle, Key * errorKey);
int elektraSyncClose (Plugin * handle, Key * errorKey);
int elektraSyncGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSyncSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSyncError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (sync);

#endif

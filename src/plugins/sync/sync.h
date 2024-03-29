/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_SYNC_H
#define ELEKTRA_PLUGIN_SYNC_H

#include <kdbplugin.h>


int elektraSyncGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSyncCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

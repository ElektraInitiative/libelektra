/**
 * @file
 *
 * @brief Header for typedispatcher plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TYPEDISPATCHER_H
#define ELEKTRA_PLUGIN_TYPEDISPATCHER_H

#include <kdbplugin.h>


int elektraTypedispatcherOpen (Plugin * handle, Key * errorKey);
int elektraTypedispatcherClose (Plugin * handle, Key * errorKey);
int elektraTypedispatcherGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTypedispatcherSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTypedispatcherError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTypedispatcherCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (typedispatcher);

#endif

/**
 * @file
 *
 * @brief Header for xerces plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XERCES_H
#define ELEKTRA_PLUGIN_XERCES_H

extern "C" {
#include <kdbplugin.h>

int elektraXercesOpen (::Plugin * handle, ::Key * errorKey);
int elektraXercesClose (::Plugin * handle, ::Key * errorKey);
int elektraXercesGet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int elektraXercesSet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);

::Plugin * ELEKTRA_PLUGIN_EXPORT;
}

#endif

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

int elektraXercesOpen (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraXercesClose (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraXercesGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraXercesSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraXercesError (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraXercesCheckConfig (ckdb::Key * errorKey, ckdb::KeySet * conf);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (xerces);
}

#endif

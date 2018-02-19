/**
 * @file
 *
 * @brief Header for yanlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YANLR_H
#define ELEKTRA_PLUGIN_YANLR_H

#include <kdbplugin.h>

extern "C" {

int elektraYanlrGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraYanlrSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (yanlr);

} // end extern "C"

#endif

/**
 * @file
 *
 * @brief Header for antlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_ANTLR_H
#define ELEKTRA_PLUGIN_ANTLR_H

#include <kdbplugin.h>

extern "C" {

int elektraAntlrGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraAntlrSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (antlr);

} // end extern "C"

#endif

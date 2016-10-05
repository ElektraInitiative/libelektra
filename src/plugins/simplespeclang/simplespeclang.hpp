/**
 * @file
 *
 * @brief Header for simplespeclang plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SIMPLESPECLANG_H
#define ELEKTRA_PLUGIN_SIMPLESPECLANG_H

#include <kdbplugin.h>

extern "C" {

int elektraSimplespeclangOpen (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraSimplespeclangClose (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraSimplespeclangGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraSimplespeclangSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraSimplespeclangError (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraSimplespeclangCheckConfig (ckdb::Key * errorKey, ckdb::KeySet * conf);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (simplespeclang);
}

#endif

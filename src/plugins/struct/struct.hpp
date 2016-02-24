/**
 * @file
 *
 * @brief Headerfile of Struct checker
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_STRUCT_H
#define ELEKTRA_PLUGIN_STRUCT_H

#include <kdbplugin.h>


extern "C" {

int elektraStructOpen (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraStructClose (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraStructGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraStructSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraStructError (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (struct);
}

#endif

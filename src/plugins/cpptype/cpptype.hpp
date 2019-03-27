/**
 * @file
 *
 * @brief Header file for entry points
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_CPPTYPE_H
#define ELEKTRA_PLUGIN_CPPTYPE_H

#include <kdbplugin.h>


extern "C" {

int elektraCppTypeOpen (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraCppTypeClose (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraCppTypeGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraCppTypeSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraCppTypeError (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraCppTypeValidateKey (ckdb::Key * key, ckdb::Key * errorKey);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT;
}

#endif

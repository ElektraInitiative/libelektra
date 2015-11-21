/**
* \file
*
* @brief Header file for entry points
*
* \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/


#ifndef ELEKTRA_PLUGIN_TYPE_H
#define ELEKTRA_PLUGIN_TYPE_H

#include <kdbplugin.h>


extern "C"
{

int elektraTypeOpen(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraTypeClose(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraTypeGet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraTypeSet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraTypeError(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(type);

}

#endif

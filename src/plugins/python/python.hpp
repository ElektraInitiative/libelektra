/*
 * \copydoc python.c
 */

#ifndef ELEKTRA_PLUGIN_PYTHON_H

extern "C"
{
#include <kdbplugin.h>

int ELEKTRA_PLUGIN_FUNCTION(Python, Open)(ckdb::Plugin *handle,  ckdb::Key *errorKey);
int ELEKTRA_PLUGIN_FUNCTION(Python, Close)(ckdb::Plugin *handle, ckdb::Key *errorKey);
int ELEKTRA_PLUGIN_FUNCTION(Python, Get)(ckdb::Plugin *handle,   ckdb::KeySet *ks, ckdb::Key *parentKey);
int ELEKTRA_PLUGIN_FUNCTION(Python, Set)(ckdb::Plugin *handle,   ckdb::KeySet *ks, ckdb::Key *parentKey);
int ELEKTRA_PLUGIN_FUNCTION(Python, Error)(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(python);
}

#endif

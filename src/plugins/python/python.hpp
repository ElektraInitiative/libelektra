/*
 * \copydoc python.c
 */

#ifndef ELEKTRA_PLUGIN_PYTHON_H

#include <kdbplugin.h>

extern "C"
{

int elektraPythonOpen(ckdb::Plugin *handle,  ckdb::Key *errorKey);
int elektraPythonClose(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraPythonGet(ckdb::Plugin *handle,   ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraPythonSet(ckdb::Plugin *handle,   ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraPythonError(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(python);

}

#endif

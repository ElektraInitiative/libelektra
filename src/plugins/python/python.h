/*
 * \copydoc python.c
 */

#ifndef ELEKTRA_PLUGIN_PYTHON_H

#include <kdbplugin.h>

int elektraPythonGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraPythonSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraPythonError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(python);

#endif

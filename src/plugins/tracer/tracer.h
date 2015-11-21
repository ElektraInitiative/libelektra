/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdbplugin.h>

#include <stdio.h>

int elektraTracerOpen(Plugin *handle, Key *errorKey);
int elektraTracerClose(Plugin *handle, Key *errorKey);
int elektraTracerGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraTracerSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraTracerError(Plugin *handle, KeySet *returned, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(tracer);

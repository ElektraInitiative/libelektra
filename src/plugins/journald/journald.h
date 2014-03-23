/*
 * \copydoc journald.c
 */

#ifndef ELEKTRA_PLUGIN_JOURNALD_H
#define ELEKTRA_PLUGIN_JOURNALD_H

#include <kdbplugin.h>

int elektraJournaldGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraJournaldSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraJournaldError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(journald);

#endif

/**
 * \file
 *
 * \brief A plugin that makes use of libaugeas to read and write configuration files
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_RENAME_H
#define ELEKTRA_PLUGIN_RENAME_H

#include <kdbplugin.h>
#include <kdberrors.h>
#include <kdbproposal.h>
#include <kdbextension.h>

int elektraRenameGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraRenameSet(Plugin *handle, KeySet *ks, Key *parentKey);
Key *elektraKeyCutNamePart(const Key *key, const Key *parentKey, const char *cutPath);

Plugin *ELEKTRA_PLUGIN_EXPORT(rename);

#endif

/**
 * @file
 *
 * @brief A plugin for renaming
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_RENAME_H
#define ELEKTRA_PLUGIN_RENAME_H

#include <kdberrors.h>
#include <kdbextension.h>
#include <kdbplugin.h>

#define ELEKTRA_ORIGINAL_NAME_META "origname"

int elektraRenameGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraRenameSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
ElektraKey * elektraKeyCreateNewName (const ElektraKey * key, const ElektraKey * parentKey, const char * cutPath, const char * replaceWith, const char * toUpper,
			       const char * toLower, const int initialConversion);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

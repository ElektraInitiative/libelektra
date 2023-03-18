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

#include <elektra/kdb/errors.h>
#include <elektra/ease/old_ease.h>
#include <elektra/ease/meta.h>
#include <elektra/plugin/plugin.h>

#define ELEKTRA_ORIGINAL_NAME_META "origname"

int elektraRenameGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRenameSet (Plugin * handle, KeySet * ks, Key * parentKey);
Key * elektraKeyCreateNewName (const Key * key, const Key * parentKey, const char * cutPath, const char * replaceWith, const char * toUpper,
			       const char * toLower, const int initialConversion);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

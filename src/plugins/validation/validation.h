/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_VALIDATION_H
#define ELEKTRA_PLUGIN_VALIDATION_H

#include <regex.h>
#include <sys/types.h>

#include <kdberrors.h>
#include <kdbplugin.h>

int elektraValidationOpen (Plugin * handle, Key * errorKey);
int elektraValidationClose (Plugin * handle, Key * errorKey);
int elektraValidationGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraValidationSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraValidationError (Plugin * handle, KeySet * ks, Key * parentKey);

Key * ksLookupRE (KeySet * ks, const regex_t * regexp);

Plugin * ELEKTRA_PLUGIN_EXPORT (validation);

#endif

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_VALIDATION_H
#define ELEKTRA_PLUGIN_VALIDATION_H

#include <regex.h>
#include <sys/types.h>

#include <kdberrors.h>
#include <kdbplugin.h>

int elektraValidationOpen (Plugin * handle, ElektraKey * errorKey);
int elektraValidationClose (Plugin * handle, ElektraKey * errorKey);
int elektraValidationGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraValidationSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraValidationError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

ElektraKey * ksLookupRE (ElektraKeyset * ks, const regex_t * regexp);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

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

#include <elektra/core.h>
#include <elektra/core/errors.h>
#include <elektra/plugin/plugin.h>

int elektraValidationOpen (Plugin * handle, Key * errorKey);
int elektraValidationClose (Plugin * handle, Key * errorKey);
int elektraValidationGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraValidationSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraValidationError (Plugin * handle, KeySet * ks, Key * parentKey);

Key * ksLookupRE (KeySet * ks, const regex_t * regexp, elektraCursor startPos);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

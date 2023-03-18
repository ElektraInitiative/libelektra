/**
 * @file
 *
 * @brief Header for reference plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_REFERENCE_H
#define ELEKTRA_PLUGIN_REFERENCE_H

#include <elektra/plugin/plugin.h>

#define CHECK_REFERENCE_KEYNAME ("check/reference")
#define CHECK_REFERENCE_RESTRICT_KEYNAME ("check/reference/restrict")

#define CHECK_REFERNCE_VALUE_SINGLE ("single")
#define CHECK_REFERNCE_VALUE_RECURSIVE ("recursive")
#define CHECK_REFERNCE_VALUE_ALTERNATIVE ("alternative")

int elektraReferenceOpen (Plugin * handle, Key * errorKey);
int elektraReferenceClose (Plugin * handle, Key * errorKey);
int elektraReferenceGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraReferenceSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraReferenceError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraReferenceCheckConf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

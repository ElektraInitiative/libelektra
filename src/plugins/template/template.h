/**
 * @file
 *
 * @brief Header for template plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TEMPLATE_H
#define ELEKTRA_PLUGIN_TEMPLATE_H

#include <elektra/kdbplugin.h>


int elektraTemplateOpen (Plugin * handle, Key * errorKey);
int elektraTemplateClose (Plugin * handle, Key * errorKey);
int elektraTemplateGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTemplateSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTemplateError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTemplateCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTemplateCheckConf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

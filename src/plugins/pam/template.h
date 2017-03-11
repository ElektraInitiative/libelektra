/**
 * @file
 *
 * @brief Header for template plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TEMPLATE_H
#define ELEKTRA_PLUGIN_TEMPLATE_H

#include <kdbplugin.h>


int elektraTemplateOpen (Plugin * handle, Key * errorKey);
int elektraTemplateClose (Plugin * handle, Key * errorKey);
int elektraTemplateGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTemplateSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTemplateError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraTemplateCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (template);

#endif

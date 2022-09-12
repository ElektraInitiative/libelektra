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

#include <kdbplugin.h>


int elektraTemplateOpen (Plugin * handle, ElektraKey * errorKey);
int elektraTemplateClose (Plugin * handle, ElektraKey * errorKey);
int elektraTemplateGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraTemplateSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraTemplateError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraTemplateCommit (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraTemplateCheckConf (ElektraKey * errorKey, ElektraKeyset * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

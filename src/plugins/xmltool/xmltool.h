/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XMLTOOL_H
#define ELEKTRA_PLUGIN_XMLTOOL_H

#include <kdbplugin.h>
#include <stdio.h>


size_t ksGetCommonParentName (ElektraKeyset * ks, char * returnedCommonParent, size_t maxSize);
size_t elektraStrLen (const char * s);

int elektraXmltoolOpen (Plugin * handle, ElektraKey * errorKey);
int elektraXmltoolClose (Plugin * handle, ElektraKey * errorKey);
int elektraXmltoolGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraXmltoolSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraXmltoolError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XMLTOOL_H
#define ELEKTRA_PLUGIN_XMLTOOL_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>
#include <stdio.h>


size_t ksGetCommonParentName (KeySet * ks, char * returnedCommonParent, size_t maxSize);
size_t elektraStrLen (const char * s);

int elektraXmltoolOpen (Plugin * handle, Key * errorKey);
int elektraXmltoolClose (Plugin * handle, Key * errorKey);
int elektraXmltoolGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXmltoolSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXmltoolError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

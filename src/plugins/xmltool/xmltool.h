/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XMLTOOL_H
#define ELEKTRA_PLUGIN_XMLTOOL_H

#include <kdbplugin.h>
#include <stdio.h>


ssize_t ksGetCommonParentName (const KeySet * ks, char * returnedCommonParent, size_t maxSize);
ssize_t ksToStream (const KeySet * ks, FILE * stream, option_t options);
int ksFromXML (KeySet * ks, int fd);
size_t elektraStrLen (const char * s);

int elektraXmltoolOpen (Plugin * handle, Key * errorKey);
int elektraXmltoolClose (Plugin * handle, Key * errorKey);
int elektraXmltoolGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXmltoolSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraXmltoolError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (xmltool);

#endif

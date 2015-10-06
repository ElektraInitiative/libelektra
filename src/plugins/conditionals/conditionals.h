/**
* @file
*
* @brief Header for conditionals plugin
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#ifndef ELEKTRA_PLUGIN_CONDITIONALS_H
#define ELEKTRA_PLUGIN_CONDITIONALS_H

#include <kdbplugin.h>


int elektraConditionalsGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraConditionalsSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(conditionals);

#endif

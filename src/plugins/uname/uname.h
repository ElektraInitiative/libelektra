/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef UNAME_H
#define UNAME_H

#include <kdbextension.h>
#include <kdbplugin.h>

int elektraUnameGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraUnameSet (Plugin * handle, KeySet * ks, Key * parentKey);

#endif

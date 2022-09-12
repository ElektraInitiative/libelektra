/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef UNAME_H
#define UNAME_H

#include <kdbextension.h>
#include <kdbplugin.h>

int elektraUnameGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);
int elektraUnameSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

#endif

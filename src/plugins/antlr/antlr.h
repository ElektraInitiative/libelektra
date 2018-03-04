/**
 * @file
 *
 * @brief Header for antlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_ANTLR_H
#define ELEKTRA_PLUGIN_ANTLR_H

#include <kdbplugin.h>

int elektraAntlrGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraAntlrSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (antlr);

#endif

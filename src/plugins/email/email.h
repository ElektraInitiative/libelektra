/**
 * @file
 *
 * @brief Header for email plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_EMAIL_H
#define ELEKTRA_PLUGIN_EMAIL_H

#include <kdbplugin.h>

int elektraEmailGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraEmailSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

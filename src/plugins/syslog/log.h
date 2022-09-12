/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef ELEKTRA_PLUGIN_SYSLOG_H
#define ELEKTRA_PLUGIN_SYSLOG_H

#include <kdbplugin.h>
#include <syslog.h>

int elektraSyslogOpen (Plugin * handle, ElektraKey * parentKey);
int elektraSyslogClose (Plugin * handle, ElektraKey * parentKey);
int elektraSyslogGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraSyslogSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraSyslogError (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

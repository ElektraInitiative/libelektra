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

int elektraSyslogOpen (Plugin * handle, Key * parentKey);
int elektraSyslogClose (Plugin * handle, Key * parentKey);
int elektraSyslogGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSyslogCommit (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSyslogError (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

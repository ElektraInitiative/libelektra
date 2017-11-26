/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbplugin.h>
#include <syslog.h>


#define BACKENDNAME "syslog"
#define BACKENDVERSION "0.0.1"

int elektraSyslogOpen (Plugin * handle, Key * parentKey);
int elektraSyslogClose (Plugin * handle, Key * parentKey);
int elektraSyslogGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSyslogSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSyslogError (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (syslog);

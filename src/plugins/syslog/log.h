/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
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

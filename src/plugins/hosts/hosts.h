#include <string.h>
#include <stdio.h>
#include <errno.h>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbprivate.h>
#include <kdberrors.h>

#define BACKENDNAME "hosts"
#define BACKENDVERSION "0.0.2"

#define HOSTS_PATH "/etc/passwd"
/* Use a buffer so large that it can hold my /etc/hosts :-)
 * TODO: make it dynamic */
#define HOSTS_BUFFER_SIZE 16384
/*Test size for small buffer
#define HOSTS_BUFFER_SIZE 16 */


int elektraHostsGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraHostsSet(Plugin *handle, KeySet *ks, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(hosts);

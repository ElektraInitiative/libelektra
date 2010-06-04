#include <string.h>
#include <stdio.h>
#include <errno.h>

#include <kdbplugin.h>
#include <kdbinternal.h>

#define BACKENDNAME "hosts"
#define BACKENDVERSION "0.0.2"

#define HOSTS_PATH "/etc/passwd"
/* Use a buffer so large that it can hold my /etc/hosts :-)
 * TODO: make it dynamic */
#define HOSTS_BUFFER_SIZE 16384
/*Test size for small buffer
#define HOSTS_BUFFER_SIZE 16 */


int kdbbWriteLock (FILE *f);
int kdbbReadLock (FILE *f);
int kdbbUnlock (FILE *f);

int kdbOpen_hosts(Plugin *handle);
int kdbClose_hosts(Plugin *handle);
ssize_t kdbGet_hosts(Plugin *handle, KeySet *ks, const Key *parentKey);
ssize_t kdbSet_hosts(Plugin *handle, KeySet *ks, const Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(hosts);

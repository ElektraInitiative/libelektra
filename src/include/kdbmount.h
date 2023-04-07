/* Mount API for newbackend, currently re-implementation of old mounting process in C instead of C++ */

#ifndef KDB_MOUNT_H
#define KDB_MOUNT_H

#include <kdb.h>
#include <stdbool.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

#define DEFAULT_MOUNTPOINTS_PATH "system:/elektra/mountpoints"

/* Actual mounting functionality */
void outputMtab (KeySet * ksMountConf, bool clFirst, bool clSecond, bool clNull);
void buildBackend (KeySet * mountConf, const char * mountPoint, char * pluginsConfig, bool clForce, bool clDebug,
		    int mergeStrategy, char * resolverName, const char * path, const KeySet * plugins, bool withRecommends);
KeySet * getMountConfig (KDB * handle, const char * mountpointsPath);

#ifdef __cplusplus
}
}
#endif

#endif // KDB_MOUNT_H

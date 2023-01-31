/* Mount API for newbackend, currently re-implementation of old mounting process in C instead of C++ */

#ifndef KDB_MOUNT_H
#define KDB_MOUNT_H

#include <stdbool.h>
#include <kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

#define DEFAULT_MOUNTPOINTS_PATH "system:/elektra/mountpoints"

/* Actual mounting functionality */
/* TODO: from mountbase.cpp */
KeySet * getMountConfig (KDB * handle, Key * errorKey, const char * const mountpointsPath);
const char * cGetMountpoint (const KeySet * const mountconf, bool clInteractive);

/* TODO: from mount.cpp */
void cOutputMtab (KeySet * mountConf, bool clFirst, bool clSecond, bool clNull);
void cProcessArguments (bool clInteractive, int numArgs);
void cBuildBackend (KeySet * const mountConf, const char * const mountPoint, char * pluginsConfig, bool clForce, bool clDebug, int mergeStrategy, char * const resolverName);



/* Backend related stuff */
bool isValidMountPoint (Key * mountPoint, KeySet * mountConf);
KeySet * getBackendInfo (KeySet * mountConf);

/* TODO: from backendparser.cpp */
const KeySet * cParsePluginArguments (char * const pluginArguments, const char * const basepath);



/* Helper functions, TODO: refactor */
void cPrintWarnings (Key * error, bool printVerbose, bool printDebug);
void freeListBackendInfo (struct cListBackendInfo * const first);


#ifdef __cplusplus
}
}
#endif

#endif // KDB_MOUNT_H

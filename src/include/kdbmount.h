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

#define DEFAULT_MOUNTPOINTS_PATH "system:/elektra/mountpoints/error"


struct cBackendInfo {
	/* where the backend is mounted */
	const char * mountpoint;
	/* The configuration file path to this backend */
	const char * path;
};

/* Linked List for BackendInfo structs */
struct cListBackendInfo {
	struct cBackendInfo backendInfo;
	struct cListBackendInfo * next;
};


/* Actual mounting functionality */

/* TODO: from mountbase.cpp */
const KeySet * cReadMountConf (bool clNull, bool clFirst, bool clSecond, bool clThird, bool clVerbose, bool clDebug);
const char * cGetMountpoint (const KeySet * const mountconf, bool clInteractive);

/* TODO: from mount.cpp */
void cOutputMtab (KeySet * const mountConf, bool clFirst, bool clSecond, bool clNull);
void cProcessArguments (bool clInteractive, int numArgs);
void cBuildBackend (KeySet * const mountconf, const char * const mountpoint, bool clForce, int mergeStrategy, bool clInteractive, const char * const pluginsconfig);



/* Backend related stuff */
struct cListBackendInfo * cGetBackendInfo (KeySet * const mountConf);


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

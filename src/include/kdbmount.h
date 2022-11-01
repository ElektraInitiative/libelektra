/* Mount API for newbackend, currently re-implementation of old mounting process in C instead of C++ */

#ifndef KDB_MOUNT_H
#define KDB_MOUNT_H

#include <stdbool.h>
#include <kdb.h>


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
const KeySet * cReadMountConf (bool clNull, bool clFirst, bool clSecond, bool clThird, bool clVerbose, bool clDebug);
void cOutputMtab (const KeySet * const mountConf, bool clFirst, bool clSecond, bool clNull);

/* Backend related stuff */
struct cListBackendInfo * cGetBackendInfo (KeySet * const mountConf);

/* Helper functions, TODO: refactor */
void cPrintWarnings (Key * error, bool printVerbose, bool printDebug);

#endif // KDB_MOUNT_H

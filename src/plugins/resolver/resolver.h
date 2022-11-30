/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef PLUGIN_RESOLVER_H
#define PLUGIN_RESOLVER_H

#define _GNU_SOURCE // needed for recursive mutex

#include "shared.h"

#include <sys/stat.h>

#include <kdberrors.h>
#include <kdbplugin.h>
#include <sys/types.h>
#include <unistd.h>

#define ERROR_SIZE 1024

typedef struct _resolverHandle resolverHandle;

struct _resolverHandle
{
	int fd;				///< Descriptor to the locking file
	struct timespec mtime;		///< Previous timestamp of the file
	mode_t filemode;		///< The mode to set (from previous file)
	mode_t dirmode;			///< The mode to set for new directories
	unsigned int removalNeeded : 1; ///< Error on freshly created files need removal
	unsigned int isMissing : 1;	///< when doing kdbGet(), no file was there
	unsigned int hasExisted : 1; 	///< when doing kdbSet(), file existed before
	int timeFix;			///< time increment to use for fixing the time

	char * dirname;	 ///< directory where real+temp file is
	char * filename; ///< the full path to the configuration file
	char * tempfile; ///< temporary file storages write to

	const char * path; ///< the configuration file name as passed from config
	const char * env;  ///< environment variables to search for files
	const char * fix;  ///< add

	gid_t gid;
	uid_t uid;
};

typedef struct _resolverHandles resolverHandles;

struct _resolverHandles
{
	resolverHandle spec;
	resolverHandle dir;
	resolverHandle user;
	resolverHandle system;
};

void ELEKTRA_PLUGIN_FUNCTION (freeHandle) (ElektraResolved *);
int ELEKTRA_PLUGIN_FUNCTION (checkFile) (const char * filename);
ElektraResolved * ELEKTRA_PLUGIN_FUNCTION (filename) (elektraNamespace, const char *, ElektraResolveTempfile, Key *);

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * handle, KeySet * returned, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * handle, KeySet * ks, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

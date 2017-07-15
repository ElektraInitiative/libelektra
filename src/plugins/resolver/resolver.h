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

#include <sys/stat.h>

#include <kdbconfig.h>
#include <kdberrors.h>
#include <kdbplugin.h>
#include <kdbproposal.h>
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
	unsigned int isMissing : 1;     ///< when doing kdbGet(), no file was there
	int timeFix;			///< time increment to use for fixing the time

	char * dirname;  ///< directory where real+temp file is
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

void ELEKTRA_PLUGIN_FUNCTION (resolver, freeHandle) (ElektraResolved *);
int ELEKTRA_PLUGIN_FUNCTION (resolver, checkFile) (const char * filename);
ElektraResolved * ELEKTRA_PLUGIN_FUNCTION (resolver, filename) (elektraNamespace, const char *, ElektraResolveTempfile, Key *);

int ELEKTRA_PLUGIN_FUNCTION (resolver, open) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (resolver, close) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (resolver, get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (resolver, set) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (resolver, error) (Plugin * handle, KeySet * returned, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT (resolver);

#endif

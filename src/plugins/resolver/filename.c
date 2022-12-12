/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "resolver.h"

#include <elektra/kdbhelper.h>
#include <elektra/kdbtypes.h>
#include <errno.h>
#include <libgen.h>
#include <pwd.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>


#define POSTFIX_SIZE 50


/**
 * Check if supplied filename is ok.
 *
 * This symbol is exported and used during mounting.
 *
 * @retval 1 on success (Relative path)
 * @retval 0 on success (Absolute path)
 * @retval -1 on a non-valid file
 */
int ELEKTRA_PLUGIN_FUNCTION (checkFile) (const char * filename)
{
	if (!filename) return -1;
	if (filename[0] == '0') return -1;

	size_t size = strlen (filename);
	char * buffer = elektraMalloc (size + sizeof ("system:/"));
	strcpy (buffer, "system:/");
	strcat (buffer, filename);

	/* Because of the outbreak bugs these tests are not enough */
	Key * check = keyNew (buffer, KEY_END);
	if (!check) goto error;
	if (!strcmp (keyName (check), "system:/")) goto error;
	keyDel (check);
	elektraFree (buffer);

	/* Be strict, don't allow any .., even if it would be ok sometimes */
	if (strstr (filename, "..") != 0) return -1;

	if (filename[0] == '/') return 0;

	return 1;

error:
	keyDel (check);
	elektraFree (buffer);
	return -1;
}

static void elektraGenTempFilename (ElektraResolved * handle, ElektraResolveTempfile tmpDir)
{
	char * tmpFile = NULL;
	size_t len = 0;
	size_t tmpFilenameSize = 0;
	if (tmpDir == ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR)
	{
		tmpFilenameSize = strlen (handle->fullPath) + POSTFIX_SIZE;
		tmpFile = elektraCalloc (tmpFilenameSize);
		len = snprintf (tmpFile, tmpFilenameSize, "%s", handle->fullPath);
	}
	else if (tmpDir == ELEKTRA_RESOLVER_TEMPFILE_TMPDIR)
	{
		tmpFilenameSize = sizeof ("/tmp/") + strlen (handle->fullPath) + POSTFIX_SIZE;
		tmpFile = elektraCalloc (tmpFilenameSize);
		len = snprintf (tmpFile, tmpFilenameSize, "/tmp/%s", handle->fullPath);
	}

	struct timeval tv;
	memset (&tv, 0, sizeof (struct timeval));
	gettimeofday (&tv, 0);
	snprintf (tmpFile + len, tmpFilenameSize - len, ".%d:%ld." ELEKTRA_TIME_USEC_F ".tmp", getpid (), tv.tv_sec, tv.tv_usec);
	handle->tmpFile = tmpFile;
}

static void elektraResolveFinishByFilename (ElektraResolved * handle, ElektraResolveTempfile tmpDir)
{
	size_t filenameSize = strlen (handle->fullPath);
	char * dir = elektraMalloc (filenameSize);
	char * dup = elektraStrDup (handle->fullPath);
	strcpy (dir, dirname (dup));
	elektraFree (dup);
	handle->dirname = dir;

	switch (tmpDir)
	{
	case ELEKTRA_RESOLVER_TEMPFILE_NONE:
		return;
	case ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR:
		elektraGenTempFilename (handle, tmpDir);
		return;
	case ELEKTRA_RESOLVER_TEMPFILE_TMPDIR:
		elektraGenTempFilename (handle, tmpDir);
		return;
	}
}

static void elektraResolveUsingHome (ElektraResolved * handle, const char * home, short addPostfix)
{
	Key * canonify = keyNew ("user:/", KEY_END);
	keyAddName (canonify, home);

	size_t dirnameSize = keyGetNameSize (canonify) + sizeof ("/" KDB_DB_USER);
	char * dir = elektraMalloc (dirnameSize);

	strcpy (dir, keyName (canonify) + sizeof ("user:") - 1);
	if (addPostfix && handle->relPath[0] != '/')
	{
		strcat (dir, "/" KDB_DB_USER);
	}
	handle->dirname = dir;
	keyDel (canonify);
}

static char * elektraResolvePasswd (Key * warningsKey)
{
	ssize_t bufSize = sysconf (_SC_GETPW_R_SIZE_MAX);
	if (bufSize == -1) bufSize = 16384; // man 3 getpwuid

	char * buf = elektraMalloc (bufSize);
	if (!buf) return NULL;
	struct passwd pwd;
	struct passwd * result;

	int s = getpwuid_r (getuid (), &pwd, buf, bufSize, &result);
	if (result == NULL)
	{
		elektraFree (buf);
		if (s != 0)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (warningsKey, "Could not retrieve from passwd using getpwuid_r. Reason: %s",
							   strerror (s));
		}
		return NULL;
	}
	char * resolved = elektraStrDup (pwd.pw_dir);
	elektraFree (buf);
	return resolved;
}

static int elektraResolveUserPasswd (ElektraResolved * handle, Key * warningsKey)
{
	char * dir = elektraResolvePasswd (warningsKey);
	if (!dir) return 0;
	elektraResolveUsingHome (handle, dir, 1);
	elektraFree (dir);
	return 1;
}

static int elektraResolveSystemPasswd (ElektraResolved * handle, Key * warningsKey)
{
	char * dir = elektraResolvePasswd (warningsKey);
	if (!dir) return -1;
	size_t filenameSize = elektraStrLen (dir) + elektraStrLen (handle->relPath) - 1;
	char * resolved = elektraMalloc (filenameSize);
	snprintf (resolved, filenameSize, "%s/%s", dir, handle->relPath + 2);
	elektraFree (dir);
	handle->fullPath = resolved;
	return 0;
}

static int elektraResolveUserXDGHome (ElektraResolved * handle, Key * warningsKey)
{
	const char * home = getenv ("XDG_CONFIG_HOME");

	if (!home || !strcmp (home, ""))
	{
		return 0;
	}

	if (home[0] != '/')
	{
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (warningsKey,
							   "XDG_CONFIG_HOME contains a path that is "
							   "not absolute (violates XDG specification) and thus "
							   "it was skipped: %s",
							   home);
		return 0;
	}
	elektraResolveUsingHome (handle, home, 0);
	return 1;
}

static int elektraResolveEnvHome (ElektraResolved * handle, Key * warningsKey)
{
	const char * home = getenv ("HOME");

	if (!home || !strcmp (home, ""))
	{
		return 0;
	}

	if (home[0] != '/')
	{
		ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (warningsKey,
							   "HOME contains a path that is "
							   "not absolute and thus "
							   "it was skipped: %s",
							   home);
		return 0;
	}
	elektraResolveUsingHome (handle, home, 1);
	return 1;
}

static int elektraResolveEnvUser (ElektraResolved * handle)
{
	const char * user = getenv ("USER");

	if (!user || !strcmp (user, ""))
	{
		return 0;
	}

	Key * canonify = keyNew ("user:/", KEY_END);
	keyAddName (canonify, user);
	size_t homeSize = sizeof (KDB_DB_HOME "/") + keyGetNameSize (canonify) + sizeof ("/" KDB_DB_USER);

	char * homeBuf = elektraMalloc (homeSize);
	strcpy (homeBuf, KDB_DB_HOME "/");
	strcat (homeBuf, keyName (canonify) + 6); // cut user:/
	if (handle->relPath[0] != '/')
	{
		strcat (homeBuf, "/" KDB_DB_USER);
	}
	keyDel (canonify);
	handle->dirname = homeBuf;
	return 1;
}

static int elektraResolveUserBuildin (ElektraResolved * handle)
{
	size_t homeSize = sizeof (KDB_DB_HOME "/") + sizeof ("/" KDB_DB_USER);

	char * homeBuf = elektraMalloc (homeSize);
	snprintf (homeBuf, homeSize, "%s", KDB_DB_HOME);
	if (handle->relPath[0] != '/')
	{
		strcat (homeBuf, "/" KDB_DB_USER);
	}
	handle->dirname = homeBuf;
	return 1;
}

static int elektraResolveUser (char variant, ElektraResolved * handle, Key * warningsKey)
{
	switch (variant)
	{
	case 'p':
		return elektraResolveUserPasswd (handle, warningsKey);
	case 'x':
		return elektraResolveUserXDGHome (handle, warningsKey);
	case 'h':
		return elektraResolveEnvHome (handle, warningsKey);
	case 'u':
		return elektraResolveEnvUser (handle);
	case 'b':
		return elektraResolveUserBuildin (handle);
	}
	return -1;
}

static void elektraResolveFinishByDirname (ElektraResolved * handle, ElektraResolveTempfile tmpDir)
{
	size_t filenameSize = elektraStrLen (handle->relPath) + elektraStrLen (handle->dirname);
	char * filename = elektraMalloc (filenameSize);
	strcpy (filename, handle->dirname);
	if (handle->relPath[0] != '/')
	{
		strcat (filename, "/");
	}
	strcat (filename, handle->relPath);
	elektraFree (handle->dirname);
	handle->fullPath = filename;
	elektraResolveFinishByFilename (handle, tmpDir);
}

static int elektraResolveMapperUser (ElektraResolved * handle, ElektraResolveTempfile tmpDir, Key * warningsKey)
{
	int finished = 0;
	size_t i;
	for (i = 0; !finished && i < sizeof (ELEKTRA_VARIANT_USER); ++i)
	{
		finished = elektraResolveUser (ELEKTRA_VARIANT_USER[i], handle, warningsKey);
	}
	if (finished == -1)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (warningsKey, "User resolver failed at step %zu, the configuration is: %s", i,
							 ELEKTRA_VARIANT_USER);
		return -1;
	}

	if (!(handle->dirname))
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (warningsKey, "No resolver set the user dirname, the configuration is: %s",
						   ELEKTRA_VARIANT_USER);
		return -1;
	}

	elektraResolveFinishByDirname (handle, tmpDir);

	return finished;
}

static int elektraResolveSystemBuildin (ElektraResolved * handle, ElektraResolveTempfile tmpDir, Key * warningsKey)
{
	size_t filenameSize = sizeof (KDB_DB_SYSTEM) + elektraStrLen (handle->relPath) + sizeof ("/");
	char * resolved = NULL;
	if (KDB_DB_SYSTEM[0] == '~')
	{
		char * resolvedPath = elektraMalloc (filenameSize);
		strcpy (resolvedPath, KDB_DB_SYSTEM);
		strcat (resolvedPath, "/");
		strcat (resolvedPath, handle->relPath);
		char * oldPath = handle->relPath;
		handle->relPath = resolvedPath;
		elektraResolveSystemPasswd (handle, warningsKey);
		elektraFree (resolvedPath);
		handle->relPath = oldPath;
	}
	else
	{
		resolved = elektraMalloc (filenameSize);
		strcpy (resolved, KDB_DB_SYSTEM);
		strcat (resolved, "/");
		strcat (resolved, handle->relPath);
		handle->fullPath = resolved;
	}
	elektraResolveFinishByFilename (handle, tmpDir);
	return 1;
}

static void elektraResolveSystemXDGHelper (char ** filename, const char * path, const char * result)
{
	size_t configDirSize = elektraStrLen (result);
	size_t pathSize = elektraStrLen (path);
	size_t filenameSize = configDirSize + pathSize + sizeof ("/") + 1;

	elektraRealloc ((void **) filename, filenameSize);
	strcpy (*filename, result);
	strcat (*filename, "/");
	strcat (*filename, path);
}

static int elektraResolveSystemXDG (ElektraResolved * handle, ElektraResolveTempfile tmpDir, Key * warningsKey)
{
	const char * configDir = getenv ("XDG_CONFIG_DIRS");
	const char * defaultDir = "/etc/xdg";
	char * filename = NULL;
	if (!configDir || !strcmp (configDir, ""))
	{
		elektraResolveSystemXDGHelper (&filename, handle->relPath, defaultDir);
		handle->fullPath = filename;
		elektraResolveFinishByFilename (handle, tmpDir);
		return 1;
	}

	char * saveptr = 0;
	char * str = elektraStrDup (configDir);
	char * result = strtok_r (str, ":", &saveptr);
	struct stat buf;
	int errnoSave = errno;
	int success = 0;
	while (result)
	{
		if (result[0] != '/')
		{
			ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (warningsKey,
								   "XDG_CONFIG_DIRS contains a path that is "
								   "not absolute (violates XDG specification) and thus "
								   "it was skipped: %s",
								   result);

			result = strtok_r (0, ":", &saveptr);
			continue;
		}

		success = 1; // at least once we got a valid path

		elektraResolveSystemXDGHelper (&filename, handle->relPath, result);

		if (stat (filename, &buf) == 0)
		{
			// we found a file!
			break;
		}

		result = strtok_r (0, ":", &saveptr);
	}
	elektraFree (str);
	errno = errnoSave;

	if (!success)
	{
		elektraResolveSystemXDGHelper (&filename, handle->relPath, defaultDir);
	}
	handle->fullPath = filename;
	elektraResolveFinishByFilename (handle, tmpDir);
	return 1;
}

/**
 * @retval 0 if variant did not have a result
 * @retval 1 on success
 */
static int elektraResolveSystem (char variant, ElektraResolved * handle, ElektraResolveTempfile tmpDir, Key * warningsKey)
{
	// hardcoded path wins against variants for now
	if (handle->relPath[0] == '/')
	{
		/* Use absolute path */
		handle->fullPath = elektraStrDup (handle->relPath);
		elektraResolveFinishByFilename (handle, tmpDir);
		return 1;
	}
	if (handle->relPath[0] == '~')
	{
		if (elektraResolveSystemPasswd (handle, warningsKey) == -1)
		{
			return -1;
		}
		elektraResolveFinishByFilename (handle, tmpDir);
		return 1;
	}
	switch (variant)
	{
	case 'x':
		return elektraResolveSystemXDG (handle, tmpDir, warningsKey);
	case 'b':
		return elektraResolveSystemBuildin (handle, tmpDir, warningsKey);
		// TODO: also document in doc/COMPILE.md
	}
	return -1;
}
static int elektraResolveMapperSystem (ElektraResolved * handle, ElektraResolveTempfile tmpDir, Key * warningsKey)
{
	int finished = 0;
	size_t i;
	for (i = 0; !finished && i < sizeof (ELEKTRA_VARIANT_SYSTEM); ++i)
	{
		finished = elektraResolveSystem (ELEKTRA_VARIANT_SYSTEM[i], handle, tmpDir, warningsKey);
	}
	if (finished == -1)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (warningsKey, "No resolver set the user dirname, the configuration is: %s",
						   ELEKTRA_VARIANT_USER);
		return -1;
	}

	if (!(handle->fullPath))
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (warningsKey, "No resolver set the system dirname, the configuration is: %s",
						   ELEKTRA_VARIANT_SYSTEM);
		return -1;
	}

	return finished;
}

/**
 * @return freshly allocated buffer with current working directory
 *
 * @param warningsKey where warnings are added
 */
static char * elektraGetCwd (Key * warningsKey)
{
	int size = 4096;
	char * cwd = elektraMalloc (size);
	if (cwd == NULL)
	{
		ELEKTRA_ADD_RESOURCE_WARNING (warningsKey, "Could not alloc for getcwd, defaulting to /");
		return 0;
	}

	char * ret = NULL;
	while (ret == NULL)
	{
		ret = getcwd (cwd, size);

		if (ret == NULL)
		{
			if (errno != ERANGE)
			{
				// give up, we cannot handle the problem
				elektraFree (cwd);
				ELEKTRA_ADD_RESOURCE_WARNINGF (warningsKey, "Method 'getcwd()' failed. Defaulting to /. Reason: %s",
							       strerror (errno));
				return 0;
			}

			// try to double the space
			size *= 2;
			elektraRealloc ((void **) &cwd, size);
			if (cwd == NULL)
			{
				ELEKTRA_ADD_RESOURCE_WARNINGF (warningsKey, "Could not realloc for `getcwd()` size %d, defaulting to /",
							       size);
				return 0;
			}
		}
	}

	return ret;
}


static int elektraResolveSpec (ElektraResolved * handle, ElektraResolveTempfile tmpDir, Key * warningsKey ELEKTRA_UNUSED)
{
	size_t filenameSize = sizeof (KDB_DB_SPEC) + strlen (handle->relPath) + sizeof ("/") + 1;
	if (handle->relPath[0] == '/')
	{
		char * filename = NULL;
		filename = elektraMalloc (filenameSize);
		strcpy (filename, handle->relPath);
		handle->fullPath = filename;
	}
	else if (handle->relPath[0] == '~')
	{
		if (elektraResolveSystemPasswd (handle, warningsKey) == -1)
		{
			return -1;
		}
	}
	else if (KDB_DB_SPEC[0] == '~')
	{
		char * oldPath = handle->relPath;
		char * path = elektraMalloc (filenameSize);
		strcpy (path, KDB_DB_SPEC);
		strcat (path, "/");
		strcat (path, handle->relPath);
		handle->relPath = path;
		elektraResolveSystemPasswd (handle, warningsKey);
		elektraFree (path);
		handle->relPath = oldPath;
	}
	else
	{
		char * path = elektraMalloc (filenameSize);
		strcpy (path, KDB_DB_SPEC);
		strcat (path, "/");
		strcat (path, handle->relPath);
		handle->fullPath = path;
	}

	elektraResolveFinishByFilename (handle, tmpDir);
	return 1;
}


static int elektraResolveDir (ElektraResolved * handle, ElektraResolveTempfile tmpDir, Key * warningsKey)
{
	char * cwd = elektraGetCwd (warningsKey);
	if (!cwd)
	{
		cwd = elektraStrDup ("/");
	}
	char * dn = elektraStrDup (cwd);
	char * dnOrig = dn;

	char * filename;

	while (true)
	{
		// now put together the filename
		filename = handle->relPath[0] == '/' ? elektraFormat ("%s%s", dn, handle->relPath) :
							     elektraFormat ("%s/" KDB_DB_DIR "/%s", dn, handle->relPath);

		struct stat buf;
		if (stat (filename, &buf) == 0)
		{
			// we found a file!
			break;
		}

		if (!strcmp (dn, "/"))
		{
			// we reached the end, filename not useful anymore
			break;
		}

		elektraFree (filename);
		dn = dirname (dn);
	}

	if (!strcmp (dn, "/"))
	{
		// nothing found, so we use most specific
		elektraFree (filename);
		filename = handle->relPath[0] == '/' ? elektraFormat ("%s%s", cwd, handle->relPath) :
							     elektraFormat ("%s/" KDB_DB_DIR "/%s", cwd, handle->relPath);
	}

	elektraFree (cwd);
	elektraFree (dnOrig);
	handle->fullPath = filename;
	elektraResolveFinishByFilename (handle, tmpDir);
	return 1;
}

void ELEKTRA_PLUGIN_FUNCTION (freeHandle) (ElektraResolved * handle)
{
	if (!handle) return;
	if (handle->relPath != NULL) elektraFree (handle->relPath);
	if (handle->dirname != NULL) elektraFree (handle->dirname);
	if (handle->fullPath != NULL) elektraFree (handle->fullPath);
	if (handle->tmpFile != NULL) elektraFree (handle->tmpFile);
	elektraFree (handle);
	handle = NULL;
}

ElektraResolved * ELEKTRA_PLUGIN_FUNCTION (filename) (elektraNamespace namespace, const char * path, ElektraResolveTempfile tmpDir,
						      Key * warningsKey)
{

	ElektraResolved * handle = elektraCalloc (sizeof (ElektraResolved));
	handle->relPath = elektraStrDup (path);

	int rc = 0;

	switch (namespace)
	{
	case KEY_NS_SPEC:
		rc = elektraResolveSpec (handle, tmpDir, warningsKey);
		break;
	case KEY_NS_DIR:
		rc = elektraResolveDir (handle, tmpDir, warningsKey);
		break;
	case KEY_NS_USER:
		rc = elektraResolveMapperUser (handle, tmpDir, warningsKey);
		break;
	case KEY_NS_SYSTEM:
		rc = elektraResolveMapperSystem (handle, tmpDir, warningsKey);
		break;
	case KEY_NS_PROC:
		ELEKTRA_ADD_INTERFACE_WARNING (warningsKey, "Resolver was not able to resolve a filename. Tried to resolve proc");
		rc = -1;
		break;
	case KEY_NS_NONE:
		ELEKTRA_ADD_INTERFACE_WARNING (warningsKey, "Resolver was not able to resolve a filename. Tried to resolve none");
		rc = -1;
		break;
	case KEY_NS_META:
		ELEKTRA_ADD_INTERFACE_WARNING (warningsKey, "Resolver was not able to resolve a filename. Tried to resolve meta");
		rc = -1;
		break;
	case KEY_NS_CASCADING:
		ELEKTRA_ADD_INTERFACE_WARNING (warningsKey, "Resolver was not able to resolve a filename. Tried to resolve cascading");
		rc = -1;
		break;
	case KEY_NS_DEFAULT:
		ELEKTRA_ADD_INTERFACE_WARNING (warningsKey, "Resolver was not able to resolve a filename. Tried to resolve default");
		rc = -1;
		break;
	}
	if (rc == -1)
	{
		ELEKTRA_PLUGIN_FUNCTION (freeHandle) (handle);
		return NULL;
	}
	return handle;
}

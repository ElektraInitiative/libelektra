#include "resolver.h"

#include <pwd.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <libgen.h>
#include <stdio.h>
#include <errno.h>
#include <stdbool.h>
#include <kdbproposal.h>

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
int ELEKTRA_PLUGIN_FUNCTION(resolver,checkFile)(const char* filename)
{
	if(!filename) return -1;
	if(filename[0] == '0') return -1;

	size_t size = strlen(filename);
	char *buffer = malloc(size + sizeof ("system/"));
	strcpy(buffer, "system/");
	strcat(buffer, filename);

	/* Because of the outbreak bugs these tests are not enough */
	Key *check = keyNew(buffer, KEY_END);
	if(!strcmp(keyName(check), "")) goto error;
	if(!strcmp(keyName(check), "system")) goto error;
	keyDel(check);
	free(buffer);

	/* Be strict, don't allow any .., even if it would be ok sometimes */
	if(strstr (filename, "..") != 0) return -1;

	if(filename[0] == '/') return 0;

	return 1;

error:
	keyDel (check);
	free (buffer);
	return -1;
}

/**
 * @brief Create unique postfix for temporary files
 *
 * Write max strlen(name)+POSTFIX_SIZE-1 characters to where.
 *
 * Truncation will begin with the least important bytes.
 *
 * @param where to write the string
 * @param filename the filename to prefix
 */
static void elektraGenTempFilename(char *where, const char *filename)
{
	struct timeval tv;
	gettimeofday(&tv, 0);
	size_t len = sprintf (where, "%s", filename);
	snprintf (where+len, POSTFIX_SIZE-1,
			".%d:%ld.%ld.tmp",
			getpid(),
			tv.tv_sec,
			tv.tv_usec);
}
/**
 * @brief Given filename, calcualtes dirname+tempfile
 *
 * @param p resolverHandle with filename set
 */
static void elektraResolveFinishByFilename(resolverHandle *p)
{
	size_t filenameSize = strlen(p->filename);
	p->dirname = malloc (filenameSize);
	char * dup = strdup(p->filename);
	//dirname might change the buffer, so better work on a copy
	strcpy (p->dirname, dirname(dup));
	free(dup);

	p->tempfile = malloc (filenameSize + POSTFIX_SIZE);
	elektraGenTempFilename(p->tempfile, p->filename);
}


static int elektraResolveSystemBuildin(resolverHandle *p)
{
	size_t filenameSize = sizeof(KDB_DB_SYSTEM)
		+ strlen(p->path) + sizeof("/") + 1;
	p->filename = malloc (filenameSize);
	strcpy (p->filename, KDB_DB_SYSTEM);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);

	elektraResolveFinishByFilename(p);
	return 1;
}

static void elektraResolveSystemXDGHelper(resolverHandle *p, const char *result)
{
	size_t configDirSize = elektraStrLen(result);
	size_t pathSize = elektraStrLen(p->path);
	size_t filenameSize = configDirSize
		+ pathSize + sizeof("/") + 1;

	elektraRealloc ((void*)&p->filename, filenameSize);
	strcpy (p->filename, result);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);
}

static int elektraResolveSystemXDG(resolverHandle *p,
		Key *warningsKey)
{
	const char * configDir = getenv("XDG_CONFIG_DIRS");
	const char *defaultDir = "/etc/xdg";

	if (!configDir || !strcmp(configDir, ""))
	{
		elektraResolveSystemXDGHelper(p, defaultDir);
		elektraResolveFinishByFilename(p);
		return 1;
	}

	char *saveptr = 0;
	char *str = elektraStrDup(configDir);
	char *result = strtok_r (str, ":", &saveptr);
	struct stat buf;
	int errnoSave = errno;
	int success = 0;
	while (result)
	{
		if (result[0] != '/')
		{
			ELEKTRA_ADD_WARNINGF(100,
				warningsKey,
				"XDG_CONFIG_DIRS contains a path that is "
				"not absolute (violates XDG specification) and thus "
			 	"it was skipped: %s",
				result);

			result = strtok_r (0, ":", &saveptr);
			continue;
		}

		success = 1; // at least once we got a valid path

		elektraResolveSystemXDGHelper(p, result);

		if (stat(p->filename, &buf) == 0)
		{
			// we found a file!
			break;
		}

		result = strtok_r (0, ":", &saveptr);
	}
	elektraFree(str);
	errno = errnoSave;

	if (!success)
	{
		elektraResolveSystemXDGHelper(p, defaultDir);
	}

	elektraResolveFinishByFilename(p);
	return 1;
}

/**
 * @retval 0 if variant did not have a result
 * @retval 1 on success
 */
static int elektraResolveSystem(char variant, resolverHandle *p, Key *warningsKey)
{
	// hardcoded path wins against variants for now
	if (p->path[0] == '/')
	{
		/* Use absolute path */
		size_t filenameSize = strlen(p->path) + 1;
		p->filename = malloc (filenameSize);
		strcpy (p->filename, p->path);

		elektraResolveFinishByFilename(p);
		return 1;
	}

	switch (variant)
	{
	case 'x':
		return elektraResolveSystemXDG(p, warningsKey);
	case 'b':
		return elektraResolveSystemBuildin(p);
	// TODO: also document in doc/COMPILE.md
	}
	return -1;
}

static void elektraResolveUsingHome(resolverHandle *p,
		const char *home,
		bool addPostfix)
{
	size_t dirnameSize = 0;
	Key *canonify = keyNew("user", KEY_END);

	keyAddName(canonify, home);

	dirnameSize = keyGetNameSize(canonify) +
			sizeof("/" KDB_DB_USER);
	p->dirname = malloc(dirnameSize);
	strcpy (p->dirname, keyName(canonify)
			+4); // cut user, but leave slash
	if (addPostfix && p->path[0] != '/')
	{
		strcat (p->dirname, "/" KDB_DB_USER);
	}
	keyDel(canonify);
}

static int elektraResolvePasswd(resolverHandle *p, Key *warningsKey)
{
	struct passwd pwd;
	struct passwd *result;
	char *buf;
	ssize_t bufsize;
	int s;

	bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
	if (bufsize == -1)          /* Value was indeterminate */
	{
		bufsize = 16384;        /* Should be more than enough */
	}

	buf = malloc(bufsize);
	if (buf == NULL) {
		return 0;
	}

	s = getpwuid_r(geteuid(), &pwd, buf, bufsize, &result);
	if (result == NULL)
	{
		free(buf);
		if (s != 0)
		{
			ELEKTRA_ADD_WARNING(90, warningsKey, strerror(s));
		}
		return 0;
	}

	/*
	printf("Info: %s; UID: %ld0, Home: %s\n",
			pwd.pw_gecos,
			(long) pwd.pw_uid,
			pwd.pw_dir);
	*/

	elektraResolveUsingHome(p, pwd.pw_dir, true);
	free(buf);

	return 1;
}

static int elektraResolveUserXDG(resolverHandle *p, Key *warningsKey)
{
	const char * home = getenv("XDG_CONFIG_HOME");

	if (!home || !strcmp(home, ""))
	{
		return 0;
	}

	if (home[0] != '/')
	{
		ELEKTRA_ADD_WARNINGF(100,
			warningsKey,
			"XDG_CONFIG_HOME contains a path that is "
			"not absolute (violates XDG specification) and thus "
			"it was skipped: %s",
			home);
		return 0;
	}

	elektraResolveUsingHome(p, home, false);

	return 1;
}

static int elektraResolveEnvHome(resolverHandle *p, Key *warningsKey)
{
	const char * home = getenv("HOME");

	if (!home || !strcmp(home, ""))
	{
		return 0;
	}

	if (home[0] != '/')
	{
		ELEKTRA_ADD_WARNINGF(100,
			warningsKey,
			"HOME contains a path that is "
			"not absolute and thus "
			"it was skipped: %s",
			home);
		return 0;
	}

	elektraResolveUsingHome(p, home, true);

	return 1;
}

static int elektraResolveEnvUser(resolverHandle *p)
{
	const char* owner = getenv("USER");

	if (!owner || !strcmp(owner, ""))
	{
		return 0;
	}

	Key *canonify = keyNew("user", KEY_END);
	keyAddName(canonify, owner);
	size_t dirnameSize = sizeof(KDB_DB_HOME "/")
			+ keyGetNameSize(canonify)
			+ sizeof("/" KDB_DB_USER);

	p->dirname= malloc (dirnameSize);
	strcpy (p->dirname, KDB_DB_HOME "/");
	strcat (p->dirname, keyName(canonify)
			+5); // cut user/
	if (p->path[0] != '/')
	{
		strcat (p->dirname, "/" KDB_DB_USER);
	}
	keyDel(canonify);

	return 1;
}


static int elektraResolveBuildin(resolverHandle *p)
{
	size_t dirnameSize = sizeof(KDB_DB_HOME "/")
		+ sizeof("/" KDB_DB_USER);

	p->dirname= malloc (dirnameSize);
	strcpy (p->dirname, KDB_DB_HOME);
	if (p->path[0] != '/')
	{
		strcat (p->dirname,  "/" KDB_DB_USER);
	}

	return 1;
}

static int elektraResolveSpec(resolverHandle *p, Key *warningsKey ELEKTRA_UNUSED)
{
	size_t filenameSize = sizeof(KDB_DB_SPEC)
		+ strlen(p->path) + sizeof("/") + 1;
	p->filename = malloc (filenameSize);
	if (p->path[0] == '/')
	{
		strcpy (p->filename, p->path);
	}
	else
	{
		strcpy (p->filename, KDB_DB_SPEC);
		strcat (p->filename, "/");
		strcat (p->filename, p->path);
	}

	elektraResolveFinishByFilename(p);
	return 1;
}

/**
 * @brief Recalculates all pathes given p->dirname
 *
 * p->filename = p->dirname+p->path
 * p->dirname = dirname(p->filename)
 *
 * @param p resolverHandle with dirname set
 */
static void elektraResolveFinishByDirname(resolverHandle *p)
{
	size_t filenameSize = strlen(p->dirname)
			+ strlen(p->path) +
			+ sizeof("/");

	p->filename = malloc (filenameSize);
	strcpy (p->filename, p->dirname);
	if (p->path[0] != '/')
	{
		strcat (p->filename, "/");
	}
	strcat (p->filename, p->path);

	// p->dirname might be wrong (too short), recalculate it:
	free(p->dirname);
	elektraResolveFinishByFilename(p);
}

/**
 * @return freshly allocated buffer with current working directory
 *
 * @param warningsKey where warnings are added
 */
static char *elektraGetCwd(Key *warningsKey)
{
	int size = 4096;
	char *cwd = elektraMalloc(size);
	if (cwd == NULL)
	{
		ELEKTRA_ADD_WARNING(83, warningsKey, "could not alloc for getcwd, defaulting to /");
		return 0;
	}

	char *ret = NULL;
	while (ret == NULL)
	{
		ret = getcwd(cwd, size);

		if (ret == NULL)
		{
			if (errno != ERANGE)
			{
				// give up, we cannot handle the problem
				free(cwd);
				ELEKTRA_ADD_WARNINGF(83, warningsKey, "getcwd failed with errno %d, defaulting to /", errno);
				return 0;
			}

			// try to double the space
			size *= 2;
			elektraRealloc((void**)&cwd, size);
			if (cwd == NULL)
			{
				ELEKTRA_ADD_WARNINGF(83, warningsKey, "could not realloc for getcwd size %d, defaulting to /", size);
				return 0;
			}
		}
	}

	return ret;
}

static int elektraResolveDir(resolverHandle *p, Key *warningsKey)
{
	char * cwd = elektraGetCwd(warningsKey);
	if (!cwd) cwd = elektraStrDup("/");

	char *dn = elektraStrDup(cwd);
	char *dnOrig = dn;
	
	while (true)
	{
		// now put together the filename
		p->filename = p->path[0] == '/' ? elektraFormat("%s%s", dn, p->path) : elektraFormat("%s/" KDB_DB_DIR "/%s", dn, p->path);

		struct stat buf;
		if (stat(p->filename, &buf) == 0)
		{
			// we found a file!
			break;
		}

		if (!strcmp(dn, "/"))
		{
			// we reached the end, filename not useful anymore
			break;
		}

		elektraFree(p->filename);
		dn = dirname(dn);
	}

	if (!strcmp(dn, "/"))
	{
		// nothing found, so we use most specific
		free(p->filename);
		p->filename = p->path[0] == '/' ? elektraFormat("%s%s", cwd, p->path) : elektraFormat("%s/" KDB_DB_DIR "/%s", cwd, p->path);
	}

	elektraFree(cwd);
	elektraFree(dnOrig);
	elektraResolveFinishByFilename(p);
	return 1;
}



/**
 * @retval 0 if variant did not have a result
 * @retval 1 on success
 */
static int elektraResolveUser(char variant, resolverHandle *p, Key *warningsKey)
{
	switch (variant)
	{
	case 'p':
		return elektraResolvePasswd(p, warningsKey);
	case 'x':
		return elektraResolveUserXDG(p, warningsKey);
	case 'h':
		return elektraResolveEnvHome(p, warningsKey);
	case 'u':
		return elektraResolveEnvUser(p);
	case 'b':
		return elektraResolveBuildin(p);
	// TODO: also document in doc/COMPILE.md
	}
	return -1;
}

static int elektraResolveMapperUser(resolverHandle *p, Key *warningsKey)
{
	int finished = 0;
	size_t i;
	for (i=0; !finished && i<sizeof(ELEKTRA_VARIANT_USER); ++i)
	{
		finished = elektraResolveUser(ELEKTRA_VARIANT_USER[i],
				p, warningsKey);
	}
	if (finished == -1)
	{
		ELEKTRA_ADD_WARNINGF(83, warningsKey,
			"user resolver failed at step %zu, the configuration is: %s",
			i, ELEKTRA_VARIANT_USER);
		return -1;
	}

	if (p->dirname == 0)
	{
		ELEKTRA_ADD_WARNINGF(83, warningsKey,
			"no resolver set the user dirname, the configuration is: %s",
			ELEKTRA_VARIANT_USER);
		return -1;
	}

	elektraResolveFinishByDirname(p);

	return finished;
}

static int elektraResolveMapperSystem(resolverHandle *p, Key *warningsKey)
{
	int finished = 0;
	size_t i;
	for (i=0; !finished && i<sizeof(ELEKTRA_VARIANT_SYSTEM); ++i)
	{
		finished = elektraResolveSystem(ELEKTRA_VARIANT_SYSTEM[i],
				p, warningsKey);
	}
	if (finished == -1)
	{
		ELEKTRA_ADD_WARNINGF(83, warningsKey,
			"system resolver failed at step %zu, the configuration is: %s",
			i, ELEKTRA_VARIANT_SYSTEM);
		return -1;
	}

	if (p->dirname == 0)
	{
		ELEKTRA_ADD_WARNINGF(83, warningsKey,
			"no resolver set the system dirname, the configuration is: %s",
			ELEKTRA_VARIANT_SYSTEM);
		return -1;
	}

	return finished;
}

/**Resolve the filename.
 *
 * For system keys it must be an absolute path, or KDB_DB_SYSTEM
 * will be attached (which should always be an absolute name).
 * This is because of security: If the user can forge the
 * path it could manipulate setuid applications to use wrong
 * configuration.
 *
 * For user keys it is ok to manipulate the path with user
 * environment variables.
 * In this implementation the owner resolution works
 * like this:
 * 1.) Owner is the metadata "owner" of the key
 * 2.) The environment variable USER will be used
 * 3.) Fall back to user "test"
 * Whatever is found first, will be used.
 * Then KDB_DB_HOME + owner + KDB_DB_USER will be used as dirname.
 *
 * @exception 83 when some environment was missing
 *
 * @retval 0 if an already absolute filename could be used
 * @retval 1 if it resolved the filename successfully
 * @retval -1 on error, basically when some environment could not
 *         be found
 * warnings will be reported to warningsKey
 */
int ELEKTRA_PLUGIN_FUNCTION(resolver, filename)
	(Key* forKey, resolverHandle *p, Key *warningsKey)
{
	if (!p)
	{
		ELEKTRA_ADD_WARNING(83, warningsKey, "no p");
		return -1;
	}

	switch (keyGetNamespace(forKey))
	{
	case KEY_NS_SPEC:
		return elektraResolveSpec(p, warningsKey);
	case KEY_NS_DIR:
		return elektraResolveDir(p, warningsKey);
	case KEY_NS_USER:
		return elektraResolveMapperUser(p, warningsKey);
	case KEY_NS_SYSTEM:
		return elektraResolveMapperSystem(p, warningsKey);
	case KEY_NS_PROC:
		ELEKTRA_ADD_WARNING(83, warningsKey, "tried to resolve proc");
		return -1;
	case KEY_NS_EMPTY:
		ELEKTRA_ADD_WARNING(83, warningsKey, "tried to resolve empty");
		return -1;
	case KEY_NS_NONE:
		ELEKTRA_ADD_WARNING(83, warningsKey, "tried to resolve none");
		return -1;
	case KEY_NS_META:
		ELEKTRA_ADD_WARNING(83, warningsKey, "tried to resolve meta");
		return -1;
	case KEY_NS_CASCADING:
		ELEKTRA_ADD_WARNING(83, warningsKey, "tried to resolve cascading");
		return -1;
	}

	ELEKTRA_ADD_WARNING(83, warningsKey, "should not be reached");
	return -1;
}

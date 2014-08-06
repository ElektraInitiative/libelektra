#include "resolver.h"

#include <pwd.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <libgen.h>
#include <stdio.h>

#define POSTFIX_SIZE 50

/**
 * Check if supplied filename is ok.
 *
 * This symbol is exported and used during mounting.
 *
 * @return 1 on success (Relative path)
 * @returns 0 on success (Absolute path)
 * @return -1 on a non-valid file
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

	/* Be strict, don't allow any .., even if it would be allowed sometimes */
	if(strstr (filename, "..") != 0) return -1;


	if(filename[0] == '/') return 0;

	/* subfolders (for non-absolute filenames) currently not
	 * supported:
	 * @see resolveFilename(), dirname is not set properly then */
	if(strstr (filename, "/") != 0) return -1;

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

static int elektraResolveSystem(resolverHandle *p)
{
	if (p->path[0] == '/')
	{
		/* Use absolute path */
		size_t filenameSize = strlen(p->path) + 1;
		p->filename = malloc (filenameSize);
		strcpy (p->filename, p->path);

		p->dirname = malloc (filenameSize);
		strcpy (p->dirname, dirname(p->filename));

		// dirname may have destroyed the content of
		// filename, so write it again
		strcpy (p->filename, p->path);

		p->tempfile = malloc(filenameSize + POSTFIX_SIZE);
		elektraGenTempFilename(p->tempfile, p->filename);

		return 0;
	}
	p->dirname= malloc (sizeof(KDB_DB_SYSTEM));
	strcpy (p->dirname, KDB_DB_SYSTEM);

	size_t filenameSize = sizeof(KDB_DB_SYSTEM)
		+ strlen(p->path) + sizeof("/") + 1;
	p->filename = malloc (filenameSize);
	strcpy (p->filename, KDB_DB_SYSTEM);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);

	p->tempfile = malloc (filenameSize + POSTFIX_SIZE);
	elektraGenTempFilename(p->tempfile, p->filename);
	return 1;
}

static void elektraResolveUsingHome(resolverHandle *p, const char *home)
{
	size_t dirnameSize = 0;
	Key *canonify = keyNew("user", KEY_END);

	keyAddBaseName(canonify, home);
	dirnameSize = keyGetNameSize(canonify) +
			sizeof("/" KDB_DB_USER);
	p->dirname = malloc(dirnameSize);
	strcpy (p->dirname, keyName(canonify)
			+4); // cut user, but leave slash
	strcat (p->dirname, "/" KDB_DB_USER);
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

	elektraResolveUsingHome(p, pwd.pw_dir);

	return 1;
}

static int elektraResolveEnvHome(resolverHandle *p)
{
	const char * home = getenv("HOME");

	if (!home)
	{
		return 0;
	}

	elektraResolveUsingHome(p, home);

	return 1;
}

static int elektraResolveEnvUser(resolverHandle *p)
{
	const char* owner = getenv("USER");

	if (!owner)
	{
		return 0;
	}

	Key *canonify = keyNew("user", KEY_END);
	keyAddBaseName(canonify, owner);
	size_t dirnameSize = sizeof(KDB_DB_HOME "/")
			+ keyGetNameSize(canonify)
			+ sizeof("/" KDB_DB_USER);

	p->dirname= malloc (dirnameSize);
	strcpy (p->dirname, KDB_DB_HOME "/");
	strcat (p->dirname, keyName(canonify)
			+5); // cut user/
	strcat (p->dirname, "/" KDB_DB_USER);
	keyDel(canonify);

	return 1;
}


static int elektraResolveBuildin(resolverHandle *p)
{
	size_t dirnameSize = sizeof(KDB_DB_HOME "/")
		+ sizeof("/" KDB_DB_USER);

	p->dirname= malloc (dirnameSize);
	strcpy (p->dirname, KDB_DB_HOME "/");
	strcat (p->dirname, KDB_DB_USER);

	return 1;
}

static void elektraResolveFinish(resolverHandle *p)
{
	size_t filenameSize = strlen(p->dirname)
			+ strlen(p->path) +
			+ sizeof("/");

	p->filename = malloc (filenameSize);
	strcpy (p->filename, p->dirname);
	strcat (p->filename, "/");
	strcat (p->filename, p->path);

	p->tempfile = malloc (filenameSize + POSTFIX_SIZE);
	elektraGenTempFilename(p->tempfile, p->filename);
}


static int elektraResolveUser(char variant, resolverHandle *p, Key *warningsKey)
{
	switch (variant)
	{
	case 'p':
		return elektraResolvePasswd(p, warningsKey);
	case 'h':
		return elektraResolveEnvHome(p);
	case 'u':
		return elektraResolveEnvUser(p);
	case 'b':
		return elektraResolveBuildin(p);
	}
	return -1;
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
		return -1;
	}

	if (!strncmp(keyName(forKey), "system", 6))
	{
		return elektraResolveSystem(p);
	}
	else if (!strncmp(keyName(forKey), "user", 4))
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
			// TODO: add i and ELEKTRA_VARIANT_USER[i]
			ELEKTRA_SET_ERROR(83, warningsKey,
				"resolver failed, the configuration is: " ELEKTRA_VARIANT_USER);
			return -1;
		}

		if (p->dirname == 0)
		{
			ELEKTRA_SET_ERROR(83, warningsKey,
				"no resolver set the dirname, the configuration is: " ELEKTRA_VARIANT_USER);
			return -1;
		}

		elektraResolveFinish(p);

		return finished;
	}

	return -1;
}

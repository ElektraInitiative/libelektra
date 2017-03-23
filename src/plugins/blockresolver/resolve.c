#define _POSIX_SOURCE

#include "resolve.h"

#include <kdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <kdbhelper.h>
#include <kdberrors.h>
#include <kdbconfig.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <errno.h>
#include <string.h>

static char * elektraResolveUsingHome(const char *home, const char *path, short addPostfix)
{
    Key * canonify = keyNew("user", KEY_END);
    keyAddName(canonify, home);
    size_t dirnameSize = keyGetNameSize(canonify) + sizeof ("/" KDB_DB_USER);
    char *dirname = elektraMalloc(dirnameSize);

    snprintf(dirname, dirnameSize, "%s", keyName(canonify) + 4);
    if (addPostfix && path[0] != '/')
    {
	strcat (dirname, "/" KDB_DB_USER);
    }
    keyDel(canonify);
    return dirname;
}

static char * elektraResolvePasswd(void)
{
    ssize_t bufSize = sysconf (_SC_GETPW_R_SIZE_MAX);
    if (bufSize == -1) bufSize = 16384; // man 3 getpwuid

    char * buf = elektraMalloc (bufSize);
    if (!buf) return NULL;
    struct passwd pwd;
    struct passwd * result;

    getpwuid_r (getuid (), &pwd, buf, bufSize, &result);
    if (result == NULL)
    {
	elektraFree (buf);
	return NULL;
    }
    char *resolved = elektraStrDup(pwd.pw_dir);
    elektraFree(buf); 
    return resolved;
}

static char * elektraResolveUserPasswd(const char *path)
{
    char * dir = elektraResolvePasswd();
    if(!dir)
	return NULL;
    char * resolved = elektraResolveUsingHome(dir, path, 1);
    elektraFree (dir);
    return resolved;
}

static char * elektraResolveSystemPasswd(const char *path)
{
    char * dir = elektraResolvePasswd();
    if(!dir)
	return NULL;
    size_t filenameSize = elektraStrLen(dir) + elektraStrLen(path) - 1;
    char * resolved = elektraMalloc(filenameSize);
    snprintf(resolved, filenameSize, "%s/%s", dir, path + 2);
    elektraFree(dir);
    return resolved;
}

static char * elektraResolveUserXDGHome(const char *path)
{
    const char * home = getenv ("XDG_CONFIG_HOME");

    if (!home || !strcmp (home, ""))
    {
	return NULL;
    }

    if (home[0] != '/')
    {
	return NULL;
    }
    char * resolved = elektraResolveUsingHome(home, path, 0);
    return resolved;
}

static char * elektraResolveEnvHome(const char *path)
{
    fprintf(stderr, "resolveEnvHome\n");
    const char * home = getenv ("HOME");

    if (!home || !strcmp (home, ""))
    {
	return NULL;
    }

    if (home[0] != '/')
    {
	return NULL;
    }
    char * resolved = elektraResolveUsingHome(home, path, 1);
    return resolved;
}

static char * elektraResolveEnvUser(const char *path)
{
    const char * user = getenv ("USER");

    if (!user || !strcmp (user, ""))
    {
	return NULL;
    }

    size_t homeSize = sizeof (KDB_DB_HOME "/") + elektraStrLen(user) + sizeof("/" KDB_DB_USER); 

    char *homeBuf = elektraMalloc(homeSize);
    snprintf(homeBuf, homeSize, "%s/%s", KDB_DB_HOME, user);
    if(path[0] != '/')
    {
	strcat(homeBuf, "/" KDB_DB_USER);
    }
    return homeBuf;
}

static char * elektraResolveUserBuildin(const char *path)
{
    size_t homeSize = sizeof (KDB_DB_HOME "/") + sizeof("/" KDB_DB_USER);

    char *homeBuf = elektraMalloc (homeSize);
    snprintf (homeBuf, homeSize, "%s", KDB_DB_HOME);
    if (path[0] != '/')
    {
	strcat (homeBuf, "/" KDB_DB_USER);
    }
    return homeBuf;
}

static char * elektraResolveUser (char variant, const char *path)
{
    switch (variant)
    {
	case 'p':
	    return elektraResolveUserPasswd (path);
	case 'x':
	    return elektraResolveUserXDGHome (path);
	case 'h':
	    return elektraResolveEnvHome (path);
	case 'u':
	    return elektraResolveEnvUser (path);
	case 'b':
	    return elektraResolveUserBuildin (path);
    }
    return NULL;
}

static void elektraResolveFinishByDirname(const char *dirname, const char *path, Key * resolveKey)
{
    size_t filenameSize = elektraStrLen(path) + elektraStrLen(dirname);
    char *filename = elektraMalloc(filenameSize);
    if(path[0] != '/')
    {
	snprintf(filename, filenameSize, "%s/%s", dirname, path);
    }
    else
    {
	snprintf(filename, filenameSize, "%s%s", dirname, path);
    }
    keySetString(resolveKey, filename);
    elektraFree(filename);
}

static Key * elektraResolveMapperUser(const char *variants, const char *path, Key * resolveKey)
{
    fprintf(stderr, "Resolving User with [%s], %s\n", variants, path);
    char * resolved = NULL;
    size_t i;
    for(i = 0; !resolved && i < sizeof(variants); ++i)
    {
	resolved = elektraResolveUser(variants[i], path);
    }
    if (!resolved)
    {
	fprintf(stderr, "Resolving Failed\n");
	keyDel(resolveKey);
	return NULL;
    }
    else
    {
	elektraResolveFinishByDirname(resolved, path, resolveKey);
	fprintf(stderr, "Resolved: %s : %s\n", resolved, keyString(resolveKey));
	elektraFree(resolved);
	return resolveKey;
    }
    keyDel(resolveKey);
    return NULL;
}

static char * elektraResolveSystemBuildin (const char *path)
{
	size_t filenameSize = sizeof (KDB_DB_SYSTEM) + elektraStrLen (path) + sizeof ("/");
	char * resolved = NULL;
	if (KDB_DB_SYSTEM[0] == '~')
	{
		char * resolvedPath = elektraMalloc (filenameSize);
		strcpy (resolvedPath, KDB_DB_SYSTEM);
		strcat (resolvedPath, "/");
		strcat (resolvedPath, path);	
		resolved = elektraResolveSystemPasswd (resolvedPath);
		elektraFree (resolvedPath);
	}
	else
	{
		resolved = elektraMalloc (filenameSize);
		strcpy (resolved, KDB_DB_SYSTEM);
		strcat (resolved, "/");
		strcat (resolved, path);
	}
	return resolved;
}

static void elektraResolveSystemXDGHelper (char ** filename, const char *path, const char *result)
{
	size_t configDirSize = elektraStrLen (result);
	size_t pathSize = elektraStrLen (path);
	size_t filenameSize = configDirSize + pathSize + sizeof ("/") + 1;

	elektraRealloc ((void *)*filename, filenameSize);
	strcpy (*filename, result);
	strcat (*filename, "/");
	strcat (*filename, path);
}

static char * elektraResolveSystemXDG (const char *path)
{
	const char * configDir = getenv ("XDG_CONFIG_DIRS");
	const char * defaultDir = "/etc/xdg";
	char * filename = NULL;
	if (!configDir || !strcmp (configDir, ""))
	{
		elektraResolveSystemXDGHelper (&filename, path, defaultDir);
		return filename;
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
			result = strtok_r (0, ":", &saveptr);
			continue;
		}

		success = 1; // at least once we got a valid path

		elektraResolveSystemXDGHelper (&filename, path, result);

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
		elektraResolveSystemXDGHelper (&filename, path, defaultDir);
	}
	return filename;
}
static char *elektraResolveSystem(char variant, const char *path)
{
    // hardcoded path wins against variants for now
    if (path[0] == '/')
    {
	/* Use absolute path */
	size_t filenameSize = strlen (path) + 1;
	char * filename = elektraMalloc (filenameSize);
	strcpy (filename, path);
	return filename;
    }
    if (path[0] == '~')
    {
	char * dir = elektraResolveSystemPasswd(path);
	if(!dir)
	    return NULL;
    	else
	    return dir;
    }
    switch (variant)
    {
	case 'x':
	    return elektraResolveSystemXDG (path);
	case 'b':
	    return elektraResolveSystemBuildin (path);
	    // TODO: also document in doc/COMPILE.md
    }
    return NULL;
}
static Key * elektraResolveMapperSystem(const char *variants, const char *path, Key *resolveKey)
{
    fprintf(stderr, "Resolving System with [%s], %s\n", variants, path);
    char * resolved = NULL;
    size_t i;
    for(i = 0; !resolved && i < sizeof(variants); ++i)
    {
	resolved = elektraResolveSystem(variants[i], path);
    }
    if (!resolved)
    {
	fprintf(stderr, "Resolving Failed\n");
	keyDel(resolveKey);
	return NULL;
    }
    else
    {
	fprintf(stderr, "Resolved: %s\n", resolved);
	keySetString(resolveKey, resolved);
	elektraFree(resolved);
	return resolveKey;
    }
    keyDel(resolveKey);
    return NULL;
}

static Key * initResolveKey(elektraNamespace Namespace)
{
    switch(Namespace)
    {
	case KEY_NS_USER:
	    return keyNew("user", KEY_END);
	case KEY_NS_SYSTEM:
	    return keyNew("system", KEY_END);
	default:
	    return NULL; 
    }
}

Key * elektraResolveFilename(const char *variants, const char * path, Key * resolveKey, elektraNamespace NameSpace)
{
    if(!path)
	return NULL;
    if(!resolveKey)
    {
	resolveKey = initResolveKey(NameSpace);
    }
    if(!resolveKey)
	return NULL;

    switch(keyGetNamespace(resolveKey))
    {
	case KEY_NS_USER:
	    return elektraResolveMapperUser(variants, path, resolveKey);
	case KEY_NS_SYSTEM:
	    return elektraResolveMapperSystem(variants, path, resolveKey);
	default:
	    return NULL;
    }

}

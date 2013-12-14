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
int resolveFilename(Key* forKey, resolverHandle *p, Key *warningsKey)
{
	if (!strncmp(keyName(forKey), "system", 6))
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
	else if (!strncmp(keyName(forKey), "user", 4))
	{
		const char * home = getenv("HOME");
		size_t dirnameSize = 0;

		if (!home)
		{
			const char* owner = getenv("USER");

			if (!owner)
			{
				dirnameSize = sizeof(KDB_DB_HOME "/")
					+ sizeof("/" KDB_DB_USER);

				p->dirname= malloc (dirnameSize);
				strcpy (p->dirname, KDB_DB_HOME "/");
				strcat (p->dirname, KDB_DB_USER);
			}
			else
			{
				Key *canonify = keyNew("user", KEY_END);
				keyAddBaseName(canonify, owner);
				dirnameSize = sizeof(KDB_DB_HOME "/")
						+ keyGetNameSize(canonify)
						+ sizeof("/" KDB_DB_USER);

				p->dirname= malloc (dirnameSize);
				strcpy (p->dirname, KDB_DB_HOME "/");
				strcat (p->dirname, keyName(canonify)
						+5); // cut user/
				strcat (p->dirname, "/" KDB_DB_USER);
				keyDel(canonify);
			}

			ELEKTRA_ADD_WARNING(83, warningsKey,
					p->dirname);
		}
		else
		{
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

		size_t filenameSize = dirnameSize
				+ strlen(p->path) +
				+ sizeof("/");

		p->filename = malloc (filenameSize);
		strcpy (p->filename, p->dirname);
		strcat (p->filename, "/");
		strcat (p->filename, p->path);

		p->tempfile = malloc (filenameSize + POSTFIX_SIZE);
		elektraGenTempFilename(p->tempfile, p->filename);

		return 1;
	}

	return -1;
}

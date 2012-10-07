#include "resolver.h"

#include <pwd.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>

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
 * 3.) The POSIX.1-2001 getlogin() will be used
 * Whatever is found first, will be used.
 * Then KDB_DB_HOME + owner + KDB_DB_USER will be used as dirname.
 *
 * @return 0 if an already absolute filename could be used
 * @return 1 if it resolved the filename successfully
 * @return -1 on error
 */
int resolveFilename(Key* forKey, resolverHandle *p)
{
	if (!strncmp(keyName(forKey), "system", 6))
	{
		if (p->path[0] == '/')
		{
			/* Use absolute path */
			size_t filenameSize = strlen(p->path) + 1 + 5;
			p->filename = malloc (filenameSize);
			strcat (p->filename, p->path);

			p->lockfile = malloc (filenameSize + 4);
			strcpy (p->lockfile, p->filename);
			strcat (p->lockfile, ".lck");

			p->tempfile = malloc (filenameSize + 4);
			strcpy (p->tempfile, p->filename);
			strcat (p->tempfile, ".tmp");

			return 0;
		}
		size_t filenameSize = sizeof(KDB_DB_SYSTEM)
			+ strlen(p->path) + 1 + 5;
		p->filename = malloc (filenameSize);
		strcpy (p->filename, KDB_DB_SYSTEM);
		strcat (p->filename, "/");
		strcat (p->filename, p->path);

		p->lockfile = malloc (filenameSize + 4);
		strcpy (p->lockfile, p->filename);
		strcat (p->lockfile, ".lck");

		p->tempfile = malloc (filenameSize + 4);
		strcpy (p->tempfile, p->filename);
		strcat (p->tempfile, ".tmp");
		return 1;
	}
	else if (!strncmp(keyName(forKey), "user", 4))
	{
		/* TODO implement XDG specification
		http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
		*/
		const Key *k = keyGetMeta(forKey, "owner");
		const char *owner = 0;
		
		if (k)
		{
			owner = keyString(k);
		}

		if (!owner)
		{
			owner = getenv("USER");
		}

		if (!owner)
		{
			owner = getlogin();
		}

		size_t filenameSize = sizeof(KDB_DB_HOME)
				+ strlen(owner)
				+ sizeof("/")
				+ sizeof("/" KDB_DB_USER "/")
				+ strlen(p->path)
				+ 5;

		p->filename = malloc (filenameSize);
		strcpy (p->filename, KDB_DB_HOME);
		strcat (p->filename, "/");
		strcat (p->filename, owner);
		strcat (p->filename, "/" KDB_DB_USER "/");
		strcat (p->filename, p->path);

		p->lockfile = malloc (filenameSize + 4);
		strcpy (p->lockfile, p->filename);
		strcat (p->lockfile, ".lck");

		p->tempfile = malloc (filenameSize + 4);
		strcpy (p->tempfile, p->filename);
		strcat (p->tempfile, ".tmp");

		return 1;
	}

	return -1;
}

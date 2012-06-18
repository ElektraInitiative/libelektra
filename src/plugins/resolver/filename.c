#include "resolver.h"

#include <pwd.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>

/**Resolve the filename.
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

		/*
		if (!owner)
		{
			struct passwd *pw = getpwuid( geteuid());
			if (pw)
			{
				owner = pw->pw_name;
			}
		}
		*/

		/* TODO for testing */
		if (!owner) owner = "test";

		if (!owner || !strcmp(owner, ""))
		{
			/* TODO implement more fallbacks (ask system/users with getuid()) */
			return -1;
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

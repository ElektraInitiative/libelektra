#include "resolver.h"

/**Resolve the filename.
 * @return 0 if an already resolved filename could be used
 * @return 1 if it resolved the filename successfully
 * @return -1 on error
 */
int resolveFilename(Key* forKey, resolverHandle *p)
{
	if (!strncmp(keyName(forKey), "system", 6))
	{
		if (p->systemFilename)
		{
			p->filename = p->systemFilename;
			return 0;
		}
		p->systemFilename = malloc (sizeof(KDB_DB_SYSTEM) + strlen(p->path) + 1);
		strcpy (p->systemFilename, KDB_DB_SYSTEM);
		strcat (p->systemFilename, "/");
		strcat (p->systemFilename, p->path);
		p->filename = p->systemFilename;
		return 1;
	}

	if (!strncmp(keyName(forKey), "user", 4))
	{
		if (p->userFilename)
		{
			p->filename = p->userFilename;
			return 0;
		}
		/* TODO implement XDG specification
		http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html

		TODO implement asking owner!
		*/
		char *home = getenv("HOME");
		if (!home)
		{
			/* TODO implement more fallbacks (ask system/users) */
			return -1;
		}

		p->userFilename = malloc (strlen(home) + strlen(p->path) + sizeof("/" KDB_DB_USER "/"));
		strcpy (p->userFilename, home);
		strcat (p->userFilename, "/" KDB_DB_USER "/");
		strcat (p->userFilename, p->path);
		p->filename = p->userFilename;
		return 1;
	}

	return -1;
}

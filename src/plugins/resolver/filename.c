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
		*/
		const char *owner = keyGetMeta(forKey, "owner");
		if (!owner)
		{
			owner = getenv ("USER");
			if (!owner)
			{
				/* TODO implement more fallbacks (ask system/users with getuid()) */
				return -1;
			}
		}

		p->userFilename = malloc (sizeof(KDB_DB_HOME)
				+ strlen(owner)
				+ sizeof("/")
				+ sizeof("/" KDB_DB_USER "/")
				+ strlen(p->path));
		strcpy (p->userFilename, KDB_DB_HOME);
		strcat (p->userFilename, "/");
		strcat (p->userFilename, owner);
		strcat (p->userFilename, "/" KDB_DB_USER "/");
		strcat (p->userFilename, p->path);
		p->filename = p->userFilename;
		return 1;
	}

	return -1;
}

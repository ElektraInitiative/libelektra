#include "resolver.h"

#include <pwd.h>
#include <sys/types.h>

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
		p->systemFilename = malloc (sizeof(KDB_DB_SYSTEM) + strlen(p->path) + 1 + 5);
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

		p->userFilename = malloc (sizeof(KDB_DB_HOME)
				+ strlen(owner)
				+ sizeof("/")
				+ sizeof("/" KDB_DB_USER "/")
				+ strlen(p->path)
				+ 5);
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

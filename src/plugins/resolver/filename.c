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
		return -1;
	}

	return -1;
}

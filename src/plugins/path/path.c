/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "path.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

int elektraPathGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	/* contract only */
	KeySet *n;
	ksAppend (returned, n=ksNew (30,
		keyNew ("system/elektra/modules/path",
			KEY_VALUE, "path plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/path/exports", KEY_END),
		keyNew ("system/elektra/modules/path/exports/get",
			KEY_FUNC, elektraPathGet,
			KEY_END),
		keyNew ("system/elektra/modules/path/exports/set",
			KEY_FUNC, elektraPathSet,
			KEY_END),
#include "readme_path.c"
		keyNew ("system/elektra/modules/path/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraPathSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	Key *cur;
	ksRewind (returned);
	while ((cur = ksNext(returned)) != 0)
	{
		const Key *meta = keyGetMeta (cur, "check/path");
		if (!meta) continue;

		struct stat buf;
		/* TODO: make exceptions configurable using path/allow */
		if (!strcmp (keyString(cur), "proc"))
		{
			continue;
		}
		else if (!strcmp (keyString(cur), "tmpfs"))
		{
			continue;
		}
		else if (!strcmp (keyString(cur), "none"))
		{
			continue;
		}
		else if (keyString(cur)[0] != '/')
		{
			ELEKTRA_SET_ERROR (56, parentKey, keyString(cur));
			return -1;
		}

		int errnosave = errno;
		if (stat(keyString(cur), &buf) == -1)
		{
			char *errmsg = elektraMalloc (ERRORMSG_LENGTH + 1 +
					+ keyGetNameSize(cur)
					+ keyGetValueSize(cur)
					+ sizeof ("name:  value:  message: "));
			strerror_r (errno, errmsg, ERRORMSG_LENGTH);
			strcat (errmsg, " from key: ");
			strcat (errmsg, keyName(cur));
			strcat (errmsg, " with path: ");
			strcat (errmsg, keyValue(cur));
			ELEKTRA_ADD_WARNING (57, parentKey, errmsg);
			free (errmsg);
			errno = errnosave;
		}
		else if (!strcmp(keyString(meta), "device"))
		{
			if (!S_ISBLK(buf.st_mode))
			{
				ELEKTRA_ADD_WARNING (54, parentKey, keyString(cur));
			}
		}
		else if (!strcmp(keyString(meta), "directory"))
		{
			if (!S_ISDIR(buf.st_mode))
			{
				ELEKTRA_ADD_WARNING (55, parentKey, keyString(cur));
			}
		}
	}

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(path)
{
	return elektraPluginExport("path",
		ELEKTRA_PLUGIN_GET,	&elektraPathGet,
		ELEKTRA_PLUGIN_SET,	&elektraPathSet,
		ELEKTRA_PLUGIN_END);
}


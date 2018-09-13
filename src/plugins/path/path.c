/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "path.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

static int validateKey (Key * key, Key * parentKey)
{
	struct stat buf;
	/* TODO: make exceptions configurable using path/allow */
	if (!strcmp (keyString (key), "proc"))
	{
		return 1;
	}
	else if (!strcmp (keyString (key), "tmpfs"))
	{
		return 1;
	}
	else if (!strcmp (keyString (key), "none"))
	{
		return 1;
	}
	else if (keyString (key)[0] != '/')
	{
		ELEKTRA_SET_ERROR (56, parentKey, keyString (key));
		return 0;
	}
	int errnosave = errno;
	const Key * meta = keyGetMeta (key, "check/path");
	if (stat (keyString (key), &buf) == -1)
	{
		char * errmsg = elektraMalloc (ERRORMSG_LENGTH + 1 + +keyGetNameSize (key) + keyGetValueSize (key) +
					       sizeof ("name:  value:  message: "));
		strerror_r (errno, errmsg, ERRORMSG_LENGTH);
		strcat (errmsg, " from key: ");
		strcat (errmsg, keyName (key));
		strcat (errmsg, " with path: ");
		strcat (errmsg, keyValue (key));
		ELEKTRA_ADD_WARNING (57, parentKey, errmsg);
		elektraFree (errmsg);
		errno = errnosave;
	}
	else if (!strcmp (keyString (meta), "device"))
	{
		if (!S_ISBLK (buf.st_mode))
		{
			ELEKTRA_ADD_WARNING (54, parentKey, keyString (key));
		}
	}
	else if (!strcmp (keyString (meta), "directory"))
	{
		if (!S_ISDIR (buf.st_mode))
		{
			ELEKTRA_ADD_WARNING (55, parentKey, keyString (key));
		}
	}


	//TODO: Integrate
	//***** To externalize *******
	const char *path = "/home/wespe";
	const char *name = "";
	const char *modes = "rwx";
	//****************************


	// Changing to specified user. Can only be done when executing user is root user
	if (strlen(name) != 0) {
		struct passwd *p;
		if ((p = getpwnam(name)) == nullptr) {
			printf("Could not find user `%s`. Does the user exist?", name);
			return -2;
		}

		int err = seteuid((int) p->pw_uid);
		if (err < 0) {
			printf("Cannot change to user %s. Are you running kdb as root? \n", name);
		}
	}

	int isRead = (strchr(modes, 'r') == nullptr) ? 0 : 1;
	int isWrite = (strchr(modes, 'w') == nullptr) ? 0 : 1;
	int isExecute = (strchr(modes, 'x') == nullptr) ? 0 : 1;

	char errorMessage[20];
	int isError = 0;

	if (euidaccess(path, R_OK) != 0) {
		isError = 1;
		strcat(errorMessage, "read,");
	}

	if (euidaccess(path, W_OK) != 0) {
		isError = 1;
		strcat(errorMessage, "write,");
	}

	if (euidaccess(path, X_OK) != 0) {
		isError = 1;
		strcat(errorMessage, "execute,");
	}

	if (isError) {
		printf("User %s does not have [%s] permission on %s", name, lastCharDel(errorMessage), path);
	}

	return 1;
}

char* lastCharDel(char* name)
{
	int i = 0;
	while(name[i] != '\0')
	{
		i++;

	}
	name[i-1] = '\0';
	return name;
}

int elektraPathGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	/* contract only */
	KeySet * n;
	ksAppend (returned, n = ksNew (30, keyNew ("system/elektra/modules/path", KEY_VALUE, "path plugin waits for your orders", KEY_END),
				       keyNew ("system/elektra/modules/path/exports", KEY_END),
				       keyNew ("system/elektra/modules/path/exports/get", KEY_FUNC, elektraPathGet, KEY_END),
				       keyNew ("system/elektra/modules/path/exports/set", KEY_FUNC, elektraPathSet, KEY_END),
				       keyNew ("system/elektra/modules/path/exports/validateKey", KEY_FUNC, validateKey, KEY_END),
#include "readme_path.c"
				       keyNew ("system/elektra/modules/path/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraPathSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* set all keys */
	Key * cur;
	ksRewind (returned);
	int rc = 1;
	while ((cur = ksNext (returned)) != 0)
	{
		const Key * meta = keyGetMeta (cur, "check/path");
		if (!meta) continue;
		rc = validateKey (cur, parentKey);
		if (!rc) return -1;
	}

	return 1; /* success */
}



Plugin * ELEKTRA_PLUGIN_EXPORT (path)
{
	// clang-format off
	return elektraPluginExport("path",
		ELEKTRA_PLUGIN_GET,	&elektraPathGet,
		ELEKTRA_PLUGIN_SET,	&elektraPathSet,
		ELEKTRA_PLUGIN_END);
}


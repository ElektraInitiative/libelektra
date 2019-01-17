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

/**
 * Used to delete last colon in the error message
 * @param name The string of which the last character should be removed
 * @return The string with the last char removed
 */
static char * lastCharDel (char * name)
{
	size_t length = elektraStrLen (name);
	name[length - 2] = '\0';
	return name;
}

/**
 * This method tries to find a matching group from a group struct containing more than one group
 * @param val The group name which is searched
 * @param groups The struct containing all groups
 * @param size The size of the groups struct because it is a linked list
 * @return true if the group is in the struct
 */
static bool isUserInGroup (unsigned int val, gid_t * groups, unsigned int size)
{
	unsigned int i;
	for (i = 0; i < size; i++)
	{
		if (groups[i] == val) return true;
	}
	return false;
}

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
		char * errmsg = elektraMalloc (ERRORMSG_LENGTH + 1 + keyGetNameSize (key) + keyGetValueSize (key) +
					       sizeof ("name:  value:  message: "));
		if (strerror_r (errno, errmsg, ERRORMSG_LENGTH) != 0)
		{
			strcpy (errmsg, "Unknown error");
		}
		strcat (errmsg, " from key: ");
		strcat (errmsg, keyName (key));
		strcat (errmsg, " with path: ");
		strcat (errmsg, keyValue (key));
		ELEKTRA_SET_ERROR (57, parentKey, errmsg);
		elektraFree (errmsg);
		errno = errnosave;
		return -1;
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
	return 1;
}

/**
 * This method validates the file permission for a certain user
 * The method assumes that the path exists and only validates permission
 * @param key The key containing all metadata
 * @param parentKey The parentKey which is used for error writing
 * @return 1 if success or -1 for failure
 */
static int validatePermission (Key * key, Key * parentKey)
{
	uid_t currentUID = geteuid ();
	gid_t currentGID = getegid ();

	const Key * userMeta = keyGetMeta (key, "check/permission/user");
	const Key * userTypes = keyGetMeta (key, "check/permission/types");

	// ***** central variables *******
	const char * validPath = keyString (key);
	const char * name = keyString (userMeta);
	const char * modes = keyString (userTypes);
	// ****************************

	struct passwd * p;

	// Changing to specified user. Can only be done when executing user is root user
	if (userMeta)
	{
		p = getpwnam (name);
		// Check if user exists
		if (p == NULL)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_USER_PERMISSION_ERROR, parentKey,
					    "Could not find user \"%s\" for key \"%s\". "
					    "Does the user exist?\"",
					    name, keyName (key));
			return -1;
		}
		name = p->pw_name;

		// Check if I can change the UID as root
		int err = seteuid ((int) p->pw_uid);
		if (err < 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_USER_PERMISSION_ERROR, parentKey,
					    "Could not set euid of user \"%s\" for key \"%s\"."
					    " Are you running kdb as root?\"",
					    name, keyName (key));
			return -1;
		}
	}
	else
	{
		uid_t uid = geteuid ();
		p = getpwuid (uid);
		name = p->pw_name;
		if (uid != 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_USER_PERMISSION_ERROR, parentKey,
					    "To check permissions for %s I need to be the root user."
					    " Are you running kdb as root?\"",
					    keyName (key));
			return -1;
		}
	}

	// The following code changes the egid if a group from a user matches the filegroup
	// TODO: Whats a good default value for ngroups
	// If the relevant group is ngroups+1 then it wont get recognized
	int ngroups = 30;
	gid_t * groups;
	groups = (gid_t *) elektraMalloc (ngroups * sizeof (gid_t));
	getgrouplist (name, (int) p->pw_gid, groups, &ngroups);

	// Get groupID of file being checked
	struct stat sb;
	stat (validPath, &sb);
	struct group * gr = getgrgid (sb.st_gid);

	bool isUserInGroupBool = isUserInGroup ((int) gr->gr_gid, groups, (unsigned int) ngroups);

	// Check if fileGroup is in userGroup. If yes change egid to that group
	if (isUserInGroupBool)
	{
		ELEKTRA_LOG_DEBUG ("User “%s” has group of file %s", name, validPath);
		int gidErr = setegid ((int) gr->gr_gid);
		if (gidErr < 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_USER_PERMISSION_ERROR, parentKey,
					    "Could not set egid of user \"%s\" for key \"%s\"."
					    " Are you running kdb as root?\"",
					    name, keyName (key));
		}
	}
	elektraFree (groups);

	// Actual checks are done
	int isRead = (strchr (modes, 'r') == NULL) ? 0 : 1;
	int isWrite = (strchr (modes, 'w') == NULL) ? 0 : 1;
	int isExecute = (strchr (modes, 'x') == NULL) ? 0 : 1;

	char errorMessage[30];
	errorMessage[0] = '\0'; // strcat() searches for this, otherwise it will print garbage chars at start
	int isError = 0;

	if (isRead && euidaccess (validPath, R_OK) != 0)
	{
		isError = 1;
		strcat (errorMessage, "read,");
	}

	if (isWrite && euidaccess (validPath, W_OK) != 0)
	{
		isError = 1;
		strcat (errorMessage, "write,");
	}

	if (isExecute && euidaccess (validPath, X_OK) != 0)
	{
		isError = 1;
		strcat (errorMessage, "execute,");
	}

	// Change back to initial effective IDs
	int euidResult = seteuid (currentUID);
	int egidResult = setegid (currentGID);

	if (euidResult != 0 || egidResult != 0)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_USER_PERMISSION_ERROR, parentKey,
				   "There was a problem in the user switching process."
				   "Please report the issue at https://issues.libelektra.org");
		return -1;
	}

	if (isError)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_PERMISSION, parentKey, "User %s does not have [%s] permission on %s", name,
				    lastCharDel (errorMessage), validPath);
		return -1;
	}

	return 1;
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
		const Key * pathMeta = keyGetMeta (cur, "check/path");
		if (!pathMeta) continue;
		rc = validateKey (cur, parentKey);
		if (rc <= 0) return -1;

		const Key * accessMeta = keyGetMeta (cur, "check/permission/types");
		if (!accessMeta) continue;
		rc = validatePermission (cur, parentKey);
		if (!rc) return -1;
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (path)
{
	// clang-format off
	return elektraPluginExport ("path",
				    ELEKTRA_PLUGIN_GET, &elektraPathGet,
				    ELEKTRA_PLUGIN_SET, &elektraPathSet,
				    ELEKTRA_PLUGIN_END);
}


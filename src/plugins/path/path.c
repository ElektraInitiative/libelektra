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

static int createModeBits (const char * modes);

static int handleNoUserCase (Key * parentKey, const char * validPath, const char * modes, int modeMask);

static int switchUser (Key * key, Key * parentKey, const char * name, const struct passwd * p);

static int switchGroup (Key * key, Key * parentKey, const char * name, const struct group * gr);

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
		ELEKTRA_ADD_WARNING (57, parentKey, errmsg);
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
 * @param key The key containing all metadata
 * @param parentKey The parentKey which is used for error writing
 * @retval 1 if success
 * @retval -1 for failure
 */
static int validatePermission (Key * key, Key * parentKey)
{

	uid_t currentUID = geteuid ();

	const Key * userMeta = keyGetMeta (key, "check/permission/user");
	const Key * userTypes = keyGetMeta (key, "check/permission/mode");

	// ***** central variables *******
	const char * validPath = keyString (key);
	const char * name = keyString (userMeta);
	const char * modes = keyString (userTypes);
	// ****************************

	int modeMask = createModeBits (modes);
	struct passwd * p;

	// Changing to specified user. Can only be done when executing user is root user
	if (userMeta && name[0] != '\0')
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
		int result = switchUser (key, parentKey, name, p);
		if (result != 0)
		{
			return result;
		}
	}

	// If user metadata is available but empty
	else if (userMeta)
	{
		return handleNoUserCase (parentKey, validPath, modes, modeMask);
	}

	// If user metadata is not given ... can only check if root can access the file
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


	gid_t * groups = (gid_t *) elektraMalloc (1 * sizeof (gid_t));
	int numberOfGroups = 1;

	// The following loop is used to get all groups for a user.
	// 4294967296 is max number of possible groups in Linux Kernel >= 2.6.3, we can only pass an integer to the method though
	// 32 for Linux Kernel < 2.6.3
	for (unsigned int i = 16; i <= UINT_MAX; i = i * 2)
	{
		int size = i;
		groups = (gid_t *) elektraMalloc (i * sizeof (gid_t));
		numberOfGroups = getgrouplist (p->pw_name, (int) p->pw_gid, groups, &size);
		if (numberOfGroups > 0)
		{
			break;
		}
		elektraFree (groups);
	}

	// Get groupID of file being checked
	struct stat sb;
	stat (validPath, &sb);
	struct group * gr = getgrgid (sb.st_gid);

	bool isUserInGroupBool = isUserInGroup ((int) gr->gr_gid, groups, (unsigned int) numberOfGroups);
	elektraFree (groups);

	// Save group so we can switch back to the original later again
	gid_t currentGID = getegid ();

	// Check if fileGroup is in userGroup. If yes change egid to that group
	if (isUserInGroupBool)
	{
		ELEKTRA_LOG_DEBUG ("User “%s” has group of file “%s“", name, validPath);
		int result = switchGroup (key, parentKey, name, gr);
		if (result != 0)
		{
			return result;
		}
	}

	// Actual check is done
	int canAccess = euidaccess (validPath, modeMask);

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

	if (canAccess != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_PERMISSION, parentKey, "User %s does not have required permission (%s) on %s",
				    name, modes, validPath);
		return -1;
	}

	return 1;
}

static int switchGroup (Key * key, Key * parentKey, const char * name, const struct group * gr)
{
	int gidErr = setegid ((int) gr->gr_gid);
	if (gidErr < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_USER_PERMISSION_ERROR, parentKey,
				    "Could not set egid of user \"%s\" for key \"%s\"."
				    " Are you running kdb as root?\"",
				    name, keyName (key));
		return -1;
	}
	return 0;
}

static int switchUser (Key * key, Key * parentKey, const char * name, const struct passwd * p)
{ // Check if I can change the UID as root
	int err = seteuid ((int) p->pw_uid);
	if (err < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_USER_PERMISSION_ERROR, parentKey,
				    "Could not set euid of user \"%s\" for key \"%s\"."
				    " Are you running kdb as root?\"",
				    name, keyName (key));
		return -1;
	}
	return 0;
}

static int handleNoUserCase (Key * parentKey, const char * validPath, const char * modes, int modeMask)
{
	struct passwd * p = getpwuid (getuid ());
	int result = access (validPath, modeMask);
	if (result != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_INVALID_PERMISSION, parentKey, "User %s does not have required permission (%s) on %s",
				    p->pw_name, modes, validPath);
		return -1;
	}
	return 1;
}

static int createModeBits (const char * modes)
{
	int modeMask = 0;
	if (strchr (modes, 'r') == NULL)
	{
		modeMask |= R_OK;
	}
	if (strchr (modes, 'w') == NULL)
	{
		modeMask |= W_OK;
	}
	if (strchr (modes, 'x') == NULL)
	{
		modeMask |= X_OK;
	}
	return modeMask;
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

		const Key * accessMeta = keyGetMeta (cur, "check/permission/mode");
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


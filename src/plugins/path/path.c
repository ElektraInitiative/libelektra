/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./path.h"

#include <internal/kdb/config.h>
#include <internal/utility/old_helper.h>

static int createModeBits (const char * modes);

static int handleNoUserCase (Key * parentKey, const char * validPath, const char * modes, Key * key);

static int switchUser (Key * key, Key * parentKey, const struct passwd * p);

static int switchGroup (Key * key, Key * parentKey, const char * name, const struct group * gr);

static int getAllGroups (Key * parentKey, uid_t currentUID, const struct passwd * p, int ngroups, gid_t ** groups);

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
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Given path '%s' should be absolute for key %s", keyString (key),
							 keyName (key));
		return 0;
	}
	int errnosave = errno;
	const Key * meta = keyGetMeta (key, "check/path");
	if (stat (keyString (key), &buf) == -1)
	{
		char * errmsg = elektraMalloc (ERRORMSG_LENGTH + 1 + keyGetNameSize (key) + keyGetValueSize (key) +
					       sizeof ("name:  value:  message: "));
		if (!errmsg) return -1;
		if (strerror_r (errno, errmsg, ERRORMSG_LENGTH) != 0)
		{
			strcpy (errmsg, "Unknown error");
		}
		strcat (errmsg, " from key: ");
		strcat (errmsg, keyName (key));
		strcat (errmsg, " with path: ");
		strcat (errmsg, keyValue (key));
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Could not find file, Reason: %s", errmsg);
		elektraFree (errmsg);
		errno = errnosave;
		return -1;
	}
	else if (!strcmp (keyString (meta), "device"))
	{
		if (!S_ISBLK (buf.st_mode))
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Device not found: %s", keyString (key));
		}
	}
	else if (!strcmp (keyString (meta), "directory"))
	{
		if (!S_ISDIR (buf.st_mode))
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Directory not found: %s", keyString (key));
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

	const Key * userMeta = keyGetMeta (key, "check/path/user");
	const Key * userTypes = keyGetMeta (key, "check/path/mode");

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
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey,
								"Could not find user '%s' for key '%s'. "
								"Does the user exist?",
								name, keyName (key));
			return -1;
		}
		name = p->pw_name;
		int result = switchUser (key, parentKey, p);
		if (result != 0)
		{
			return result;
		}
	}

	// If user metadata is available but empty
	else if (userMeta)
	{
		return handleNoUserCase (parentKey, validPath, modes, key);
	}

	// If user metadata is not given ... can only check if root can access the file
	else
	{
		uid_t uid = geteuid ();
		p = getpwuid (uid);
		name = p->pw_name;
		if (uid != 0)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey,
						     "To check permissions for %s I need to be the root user."
						     " Are you running kdb as root?",
						     keyName (key));
			return -1;
		}
	}
	int ngroups = 0;
	gid_t * groups;
	int allGroupsReturnCode = getAllGroups (parentKey, currentUID, p, ngroups, &groups);
	if (allGroupsReturnCode != 0)
	{
		return allGroupsReturnCode;
	}

	// Get groupID of file being checked
	struct stat sb;
	stat (validPath, &sb);
	struct group * gr = getgrgid (sb.st_gid);

	bool isUserInGroupBool = isUserInGroup ((int) gr->gr_gid, groups, (unsigned int) ngroups);
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
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "There was a problem in the user switching process."
					    "Please report the issue at https://issues.libelektra.org");
		return -1;
	}

	if (canAccess != 0)
	{
		// No Resource error per se because related to the specification check!
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "User %s does not have required permission (%s) on '%s'. Key: %s", name,
							modes, validPath, keyName (key));
		return -1;
	}

	return 1;
}

/**
 * This method saves all groups into the groups parameter and also saves the number of groups into ngroups
 * @param parentKey The parentKey to which error messages are logged
 * @param currentUID The current userID which is used to switch back to in case of an error
 * @param p passwd struct which has all relevant user information
 * @param ngroups the number of groups of a user. Should be initialized with 0
 * @param groups the actual groups which are returned
 * @retval 0 if success
 */
static int getAllGroups (Key * parentKey, uid_t currentUID, const struct passwd * p, int ngroups, gid_t ** groups)
{
	gid_t * tmpGroups = (gid_t *) elektraMalloc (0 * sizeof (gid_t));
	getgrouplist (p->pw_name, (int) p->pw_gid, tmpGroups, &ngroups);
	elektraFree (tmpGroups);
	(*groups) = (gid_t *) elektraMalloc (ngroups * sizeof (gid_t));
	// call to getgrouplist fails because at least one group (p->pw_gid) is returned
	// therefore ngroups now contains the actual number of groups for the user
	if (getgrouplist (p->pw_name, (int) p->pw_gid, (*groups), &ngroups) < 0)
	{
		ELEKTRA_SET_INTERNAL_ERROR (parentKey,
					    "There was a problem in the getting all groups for the user."
					    "Please report the issue at https://issues.libelektra.org");
		if (seteuid (currentUID) < 0)
		{
			ELEKTRA_SET_INTERNAL_ERROR (parentKey,
						    "There was a problem in the user switching process."
						    "Please report the issue at https://issues.libelektra.org");
		}
		return -1;
	}
	return 0;
}

/**
 * Switches the effective groupID of a user
 * @param key Used for senseful logging of where the error occurred
 * @param parentKey The parentKey to which error messages are logged
 * @param name Used for senseful logging of where the error occurred. Represents the actual username
 * @param gr The group to which it is switched
 * @retval 0 if success
 */
static int switchGroup (Key * key, Key * parentKey, const char * name, const struct group * gr)
{
	int gidErr = setegid ((int) gr->gr_gid);
	if (gidErr < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey,
					     "Could not set egid of user '%s' for key '%s'."
					     " Are you running kdb as root?",
					     name, keyName (key));
		return -1;
	}
	return 0;
}

/**
 * Switches the effective userID of a user. This method only works if the executing user has root privileged.
 * @param key Used for senseful logging of where the error occurred
 * @param parentKey The parentKey to which error messages are logged
 * @param p passwd struct which has all relevant user information
 * @retval 0 if success
 * @retval -1 if failure happens
 */
static int switchUser (Key * key, Key * parentKey, const struct passwd * p)
{
	// Check if I can change the UID as root
	int err = seteuid ((int) p->pw_uid);
	if (err < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey,
					     "Could not set euid of user '%s' for key '%s'."
					     " Are you running kdb as root?",
					     p->pw_name, keyName (key));
		return -1;
	}
	return 0;
}

/**
 * This method checks for the current executing user if he can access the file/directory given with the respective modes.
 * @param parentKey The parentKey to which error messages are logged
 * @param validPath Used for senseful logging of where the error occurred
 * @param modes The modes which should be checked for the current user
 * @retval 1 if success
 * @retval -1 if failure happens
 */
static int handleNoUserCase (Key * parentKey, const char * validPath, const char * modes, Key * key)
{
	int modeMask = createModeBits (modes);
	struct passwd * p = getpwuid (getuid ());
	int result = access (validPath, modeMask);
	if (result != 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "User '%s' does not have required permission (%s) on '%s'. Key: %s",
							p->pw_name, modes, validPath, keyName (key));
		return -1;
	}
	return 1;
}

/**
 * Takes modes given by the user (e.g. rwx) and converts it to the bitmask required for access and euidaccess
 * @param modes The modes given by the user in the metakey
 * @return The modes as bits required for access and euidaccess
 */
static int createModeBits (const char * modes)
{
	int modeMask = 0;
	if (strchr (modes, 'r') != NULL)
	{
		modeMask |= R_OK;
	}
	if (strchr (modes, 'w') != NULL)
	{
		modeMask |= W_OK;
	}
	if (strchr (modes, 'x') != NULL)
	{
		modeMask |= X_OK;
	}
	return modeMask;
}

int elektraPathGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	/* contract only */
	KeySet * n;
	ksAppend (returned, n = ksNew (30, keyNew ("system:/elektra/modules/path", KEY_VALUE, "path plugin waits for your orders", KEY_END),
				       keyNew ("system:/elektra/modules/path/exports", KEY_END),
				       keyNew ("system:/elektra/modules/path/exports/get", KEY_FUNC, elektraPathGet, KEY_END),
				       keyNew ("system:/elektra/modules/path/exports/set", KEY_FUNC, elektraPathSet, KEY_END),
				       keyNew ("system:/elektra/modules/path/exports/validateKey", KEY_FUNC, validateKey, KEY_END),

#include "./readme_path.c"

				       keyNew ("system:/elektra/modules/path/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraPathSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* set all keys */
	Key * cur;
	int rc = 1;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		cur = ksAtCursor (returned, it);
		const Key * pathMeta = keyGetMeta (cur, "check/path");
		if (!pathMeta) continue;
		rc = validateKey (cur, parentKey);
		if (rc <= 0) return -1;

		const Key * accessMeta = keyGetMeta (cur, "check/path/mode");
		if (!accessMeta) continue;
		rc = validatePermission (cur, parentKey);
		if (!rc) return -1;
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("path",
				    ELEKTRA_PLUGIN_GET, &elektraPathGet,
				    ELEKTRA_PLUGIN_SET, &elektraPathSet,
				    ELEKTRA_PLUGIN_END);
}


/**
 * @file
 *
 * @brief Methods for metadata manipulation.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <kdbconfig.h>
#include <kdbease.h>
#include <kdbmeta.h>
#include <kdbprivate.h>
#include <kdbproposal.h>
#include <kdbtypes.h>
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif


/**
 * @defgroup meta Meta Data proposal+compatibility
 * @brief Meta data proposal+compatibility methods.
 * @ingroup proposal
 *
 * In versions before Elektra 0.8 only limited metadata was
 * available. Now any metadata can be added. These API methods are
 * implementations of the 0.7 API using 0.8 metadata.
 *
 * Additionally, new suggestions can be made here.
 *
 * It is planned that these methods will be generated from doc/METADATA.ini
 * and moved to a separate library.
 * Currently, you should better avoid the methods and directly use  @link keymeta metainfo @endlink
 * instead.
 *
 * @{
 *
 */


/*********************************************
 *    General comment manipulation methods   *
 *********************************************/


/**
 * Return a pointer to the real internal @p key comment.
 *
 * This is a much more efficient version of keyGetComment() and you
 * should use it if you are responsible enough to not mess up things.
 * You are not allowed to change anything in the memory region the
 * returned pointer points to.
 *
 * keyComment() returns "" when there is no keyComment. The reason is
 * @code
 key=keyNew(0);
 keySetComment(key,"");
 keyComment(key); // you would expect "" here
 keyDel(key);
 * @endcode
 *
 * See keySetComment() for more information on comments.
 *
 * @note Note that the Key structure keeps its own size field that is calculated
 * by library internal calls, so to avoid inconsistencies, you
 * must never use the pointer returned by keyComment() method to set a new
 * value. Use keySetComment() instead.
 *
 * @param key the key object to work with
 * @return a pointer to the internal managed comment
 * @retval "" when there is no comment
 * @retval 0 on NULL pointer
 * @see keyGetCommentSize() for size and keyGetComment() as alternative
 */
const char * keyComment (const Key * key)
{
	const char * comment;

	if (!key) return 0;
	comment = keyValue (keyGetMeta (key, "comment"));

	if (!comment)
	{
		/*errno=KDB_ERR_NOKEY;*/
		return "";
	}

	return comment;
}


/**
 * Calculates number of bytes needed to store a key comment, including
 * final NULL.
 *
 * Use this method to know to size for allocated memory to retrieve
 * a key comment.
 *
 * See keySetComment() for more information on comments.
 *
 * For an empty key name you need one byte to store the ending NULL.
 * For that reason 1 is returned.
 *
 * @code
 char *buffer;
 buffer = elektraMalloc (keyGetCommentSize (key));
// use this buffer to store the comment
// pass keyGetCommentSize (key) for maxSize
 * @endcode
 *
 * @param key the key object to work with
 * @return number of bytes needed
 * @retval 1 if there is no comment
 * @retval -1 on NULL pointer
 * @see keyGetComment(), keySetComment()
 */
ssize_t keyGetCommentSize (const Key * key)
{
	ssize_t size;
	if (!key) return -1;

	size = keyGetValueSize (keyGetMeta (key, "comment"));

	if (!size || size == -1)
	{
		/*errno=KDB_ERR_NODESC;*/
		return 1;
	}

	return size;
}


/**
 * Get the key comment.
 *
 * @section comment Comments
 *
 * A Key comment is description for humans what this key is for. It may be a
 * textual explanation of valid values, when and why a user or administrator
 * changed the key or any other text that helps the user or administrator related
 * to that key.
 *
 * Don't depend on a comment in your program. A user is
 * always allowed to remove or change it in any way he wants to. But you are
 * allowed or even encouraged to always show the content of the comment
 * to the user and allow him to change it.
 *
 * @param key the key object to work with
 * @param returnedComment pre-allocated memory to copy the comments to
 * @param maxSize number of bytes that will fit returnedComment
 * @return the number of bytes actually copied to @p returnedString, including
 * 	final NULL
 * @retval 1 if the string is empty
 * @retval -1 on NULL pointer
 * @retval -1 if maxSize is 0, not enough to store the comment or when larger then SSIZE_MAX
 * @see keyGetCommentSize(), keySetComment()
 */
ssize_t keyGetComment (const Key * key, char * returnedComment, size_t maxSize)
{
	const char * comment;
	size_t commentSize;
	if (!key) return -1;

	if (!maxSize) return -1;
	if (!returnedComment) return -1;
	if (maxSize > SSIZE_MAX) return -1;

	comment = keyValue (keyGetMeta (key, "comment"));
	commentSize = keyGetValueSize (keyGetMeta (key, "comment"));

	if (!comment)
	{
		/*errno=KDB_ERR_NODESC;*/
		returnedComment[0] = 0;
		return 1;
	}

	strncpy (returnedComment, comment, maxSize);
	if (maxSize < commentSize)
	{
		/*errno=KDB_ERR_TRUNC;*/
		return -1;
	}
	return commentSize;
}


/**
 * Set a comment for a key.
 *
 * A key comment is like a configuration file comment.
 * See keySetComment() for more information.
 *
 * @param key the key object to work with
 * @param newComment the comment, that can be freed after this call.
 * @return the number of bytes actually saved including final NULL
 * @retval 0 when the comment was freed (newComment NULL or empty string)
 * @retval -1 on NULL pointer or memory problems
 * @see keyGetComment()
 */
ssize_t keySetComment (Key * key, const char * newComment)
{
	if (!key) return -1;
	if (!newComment || *newComment == 0)
	{
		keySetMeta (key, "comment", 0);
		return 1;
	}

	keySetMeta (key, "comment", newComment);
	return keyGetCommentSize (key);
}


#ifndef _WIN32

/*********************************************
 *       UID, GID access methods             *
 *********************************************/


/**
 * Get the user ID of a key.
 *
 * @deprecated This API is obsolete.
 *
 * @section UID UID
 *
 * The user ID is a unique identification for every user present on a
 * system. Keys will belong to root (0) as long as you did not get their
 * real UID with kdbGet().
 *
 * Although usually the same, the UID of a key is not related to its owner.
 *
 * A fresh key will have no UID.
 *
 * @param key the key object to work with
 * @return the system's UID of the key
 * @retval (uid_t)-1 on NULL key
 * @see keyGetGID(), keySetUID(), keyGetOwner()
 */
uid_t keyGetUID (const Key * key)
{
	const char * uid;
	long int val;
	char * endptr;
	int errorval = errno;

	if (!key) return (uid_t) -1;

	uid = keyValue (keyGetMeta (key, "uid"));
	if (!uid) return (uid_t) -1;
	if (*uid == '\0') return (uid_t) -1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol (uid, &endptr, 10);

	/*Check for errors*/
	if (errno) goto cleanup;

	/*Check if nothing was found*/
	if (endptr == uid) goto cleanup;

	/*Check if the whole string was processed*/
	if (*endptr != '\0') goto cleanup;

	return val;
cleanup:
	/*First restore errno*/
	errno = errorval;
	return (uid_t) -1;
}


/**
 * Set the user ID of a key.
 *
 * @deprecated This API is obsolete.
 *
 * See @ref UID for more information about user IDs.
 *
 * @param key the key object to work with
 * @param uid the user ID to set
 * @retval 0 on success
 * @retval -1 on NULL key or conversion error
 * @see keySetGID(), keyGetUID(), keyGetOwner()
 */
int keySetUID (Key * key, uid_t uid)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT - 1, "%d", uid) < 0)
	{
		return -1;
	}

	keySetMeta (key, "uid", str);

	return 0;
}


/**
 * Get the group ID of a key.
 *
 * @deprecated This API is obsolete.
 *
 * @section GID GID
 *
 * The group ID is a unique identification for every group present on
 * a system. Keys will belong to root (0) as long as you did not get their
 * real GID with kdbGet().
 *
 * Unlike UID users might change their group. This makes it possible to
 * share configuration between some users.
 *
 * A fresh key will have (gid_t)-1 also known as the group nogroup.
 * It means that the key is not related to a group ID at the moment.
 *
 * @param key the key object to work with
 * @return the system's GID of the key
 * @retval (gid_t)-1 on NULL key or currently unknown ID
 * @see keySetGID(), keyGetUID()
 */
gid_t keyGetGID (const Key * key)
{
	const char * gid;
	long int val;
	char * endptr;
	int errorval = errno;

	if (!key) return (gid_t) -1;

	gid = keyValue (keyGetMeta (key, "gid"));
	if (!gid) return (gid_t) -1;
	if (*gid == '\0') return (gid_t) -1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol (gid, &endptr, 10);

	/*Check for errors*/
	if (errno) goto cleanup;

	/*Check if nothing was found*/
	if (endptr == gid) goto cleanup;

	/*Check if the whole string was processed*/
	if (*endptr != '\0') goto cleanup;

	return val;
cleanup:
	/*First restore errno*/
	errno = errorval;
	return (gid_t) -1;
}


/**
 * Set the group ID of a key.
 *
 * @deprecated This API is obsolete.
 *
 * See @ref GID for more information about group IDs.
 *
 * @param key the key object to work with
 * @param gid is the group ID
 * @retval 0 on success
 * @retval -1 on NULL key
 * @see keyGetGID(), keySetUID()
 */
int keySetGID (Key * key, gid_t gid)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT - 1, "%d", gid) < 0)
	{
		return -1;
	}

	keySetMeta (key, "gid", str);

	return 0;
}


/**
 * Set mode so that key will be recognized as directory.
 *
 * @deprecated This API is obsolete.
 *
 * The function will add all executable bits.
 *
 * - Mode 0200 will be translated to 0311
 * - Mode 0400 will be translated to 0711
 * - Mode 0664 will be translated to 0775
 *
 * The macro KDB_DIR_MODE (defined to 0111) will be used for that.
 *
 * The executable bits show that child keys are allowed and listable. There
 * is no way to have child keys which are not listable for anyone, but it is
 * possible to restrict listing the keys to the owner only.
 *
 * - Mode 0000 means that it is a key not read or writable to anyone.
 * - Mode 0111 means that it is a directory not read or writable to anyone.
 *   But it is recognized as directory to anyone.
 *
 * For more about mode see keySetMode().
 *
 * It is not possible to access keys below a not executable key.
 * If a key is not writeable and executable kdbSet() will fail to access the
 * keys below.
 * If a key is not readable and executable kdbGet() will fail to access the
 * keys below.
 *
 * @param key the key to set permissions to be recognized as directory.
 * @retval 0 on success
 * @retval -1 on NULL pointer
 * @see keySetMode()
 */
int keySetDir (Key * key)
{
	mode_t mode;
	if (!key) return -1;

	mode = keyGetMode (key);
	mode |= KDB_DIR_MODE;
	keySetMode (key, mode);

	return 0;
}


/**
 * Return the key mode permissions.
 *
 * @deprecated This API is obsolete.
 *
 * Default is 0664 (octal) for keys and 0775 for directory keys
 * which used keySetDir().
 *
 * The defaults are defined with the macros KDB_FILE_MODE and KDB_DIR_MODE.
 *
 * For more information about the mode permissions see @ref mode.
 *
 * @param key the key object to work with
 * @return mode permissions of the key
 * @retval KDB_FILE_MODE as defaults
 * @retval (mode_t)-1 on NULL pointer
 * @see keySetMode()
 */
mode_t keyGetMode (const Key * key)
{
	const char * mode;
	long int val;
	char * endptr;
	int errorval = errno;

	if (!key) return (mode_t) -1;

	mode = keyValue (keyGetMeta (key, "mode"));
	if (!mode) return KDB_FILE_MODE;
	if (*mode == '\0') return KDB_FILE_MODE;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol (mode, &endptr, 8);

	/*Check for errors*/
	if (errno) goto cleanup;

	/*Check if nothing was found*/
	if (endptr == mode) goto cleanup;

	/*Check if the whole string was processed*/
	if (*endptr != '\0') goto cleanup;

	return val;
cleanup:
	/*First restore errno*/
	errno = errorval;
	return KDB_FILE_MODE;
}


/**
 * Set the key mode permissions.
 *
 * @deprecated This API is obsolete.
 * It is only a mapping
 * to keySetMeta(key, "mode", str) which should be preferred.
 *
 * The mode consists of 9 individual bits for mode permissions.
 * In the following explanation the octal notation with leading
 * zero will be used.
 *
 * Default is 0664 (octal) for keys and 0775 for directory keys
 * which used keySetDir().
 *
 * The defaults are defined with the macros KDB_FILE_MODE and KDB_DIR_MODE.
 *
 * @note libelektra 0.7.0 only allows 0775 (directory keys) and
 * 0664 (other keys). More will be added later in a sense of the
 * description below.
 *
 * @section mode Modes
 *
 * 0000 is the most restrictive mode. No user might read, write
 * or execute the key.
 *
 * Reading the key means to get the value by kdbGet().
 *
 * Writing the key means to set the value by kdbSet().
 *
 * Execute the key means to make a step deeper in the hierarchy.
 * But you must be able to read the key to be able to list the
 * keys below. See also keySetDir() in that context.
 * But you must be able to write the key to be able to add or
 * remove keys below.
 *
 * 0777 is the most relaxing mode. Every user is allowed to
 * read, write and execute the key, if he is allowed to execute
 * and read all keys below.
 *
 * 0700 allows every action for the current user, identified by
 * the uid. See keyGetUID() and keySetUID().
 *
 * To be more specific for the user the single bits can elect
 * the mode for read, write and execute. 0100 only allows
 * executing which gives the information that it is a directory
 * for that user, but not accessible. 0200 only allows reading.
 * This information may be combined to 0300, which allows execute
 * and reading of the directory. Last 0400 decides about the
 * writing permissions.
 *
 * The same as above is also valid for the 2 other octal digits.
 * 0070 decides about the group permissions, in that case full
 * access. Groups are identified by the gid. See keyGetGID() and
 * keySetGID(). In that example everyone with a different uid,
 * but the gid of the the key, has full access.
 *
 * 0007 decides about the world permissions. This is taken into
 * account when neither the uid nor the gid matches. So that
 * example would allow everyone with a different uid and gid
 * of that key gains full access.
 *
 * @param key the key to set mode permissions
 * @param mode the mode permissions
 * @retval 0 on success
 * @retval -1 on NULL key
 * @see keyGetMode()
 */
int keySetMode (Key * key, mode_t mode)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT - 1, "%o", mode) < 0)
	{
		return -1;
	}

	keySetMeta (key, "mode", str);

	return 0;
}


/*********************************************
 *    Access times methods                   *
 *********************************************/


/**
 * Get last time the key data was read from disk.
 *
 * @deprecated This API is obsolete.
 *
 * Every kdbGet() might update the access time
 * of a key. You get information when the key
 * was read the last time from the database.
 *
 * You will get 0 when the key was not read already.
 *
 * Beware that multiple copies of keys with keyDup() might have different
 * atimes because you kdbGet() one, but not the
 * other. You can use this information to decide which
 * key is the latest.
 *
 * @param key Key to get information from.
 * @return the time you got the key with kdbGet()
 * @retval 0 on key that was never kdbGet()
 * @retval (time_t)-1 on NULL pointer
 * @see keySetATime()
 * @see kdbGet()
 */
time_t keyGetATime (const Key * key)
{
	const char * atime;
	long int val;
	char * endptr;
	int errorval = errno;

	if (!key) return (time_t) -1;

	atime = keyValue (keyGetMeta (key, "atime"));
	if (!atime) return 0;
	if (*atime == '\0') return (time_t) -1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol (atime, &endptr, 10);

	/*Check for errors*/
	if (errno) goto cleanup;

	/*Check if nothing was found*/
	if (endptr == atime) goto cleanup;

	/*Check if the whole string was processed*/
	if (*endptr != '\0') goto cleanup;

	return val;
cleanup:
	/*First restore errno*/
	errno = errorval;
	return (time_t) -1;
}

/**
 * Update the atime information for a key.
 *
 * @deprecated This API is obsolete.
 *
 * When you do manual sync of keys you might also
 * update the atime to make them indistinguishable.
 *
 * It can also be useful if you work with
 * keys not using a keydatabase.
 *
 * @param key The Key object to work with
 * @param atime The new access time for the key
 * @retval 0 on success
 * @retval -1 on NULL pointer
 * @see keyGetATime()
 */
int keySetATime (Key * key, time_t atime)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT - 1, "%lu", atime) < 0)
	{
		return -1;
	}

	keySetMeta (key, "atime", str);

	return 0;
}


/**
 * Get last modification time of the key on disk.
 *
 * @deprecated This API is obsolete.
 *
 * You will get 0 when the key was not read already.
 *
 * Everytime you change value or comment and kdbSet()
 * the key the mtime will be updated. When you kdbGet()
 * the key, the atime is set appropriate.
 *
 * Not changed keys may not even passed to kdbSet_backend()
 * so it will not update this time, even after kdbSet().
 *
 * It is possible that other keys written to disc
 * influence this time if the backend is not grained
 * enough.
 *
 * If you add or remove a key the key thereunder
 * in the hierarchy will update the mtime
 * if written with kdbSet() to disc.
 *
 * @param key Key to get information from.
 * @see keySetMTime()
 * @return the last modification time
 * @retval (time_t)-1 on NULL pointer
 */
time_t keyGetMTime (const Key * key)
{
	const char * mtime;
	long int val;
	char * endptr;
	int errorval = errno;

	if (!key) return (time_t) -1;

	mtime = keyValue (keyGetMeta (key, "mtime"));
	if (!mtime) return 0;
	if (*mtime == '\0') return (time_t) -1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol (mtime, &endptr, 10);

	/*Check for errors*/
	if (errno) goto cleanup;

	/*Check if nothing was found*/
	if (endptr == mtime) goto cleanup;

	/*Check if the whole string was processed*/
	if (*endptr != '\0') goto cleanup;

	return val;
cleanup:
	/*First restore errno*/
	errno = errorval;
	return (time_t) -1;
}

/**
 * Update the mtime information for a key.
 *
 * @deprecated This API is obsolete.
 *
 * @param key The Key object to work with
 * @param mtime The new modification time for the key
 * @retval 0 on success
 * @see keyGetMTime()
 */
int keySetMTime (Key * key, time_t mtime)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT - 1, "%lu", mtime) < 0)
	{
		return -1;
	}

	keySetMeta (key, "mtime", str);

	return 0;
}


/**
 * Get last time the key metadata was changed from disk.
 *
 * @deprecated This API is obsolete.
 *
 * You will get 0 when the key was not read already.
 *
 * Any changed field in metadata will influence the
 * ctime of a key.
 *
 * This time is not updated if only value
 * or comment are changed.
 *
 * Not changed keys will not update this time,
 * even after kdbSet().
 *
 * It is possible that other keys written to disc
 * influence this time if the backend is not grained
 * enough.
 *
 * @param key Key to get information from.
 * @see keySetCTime()
 * @retval (time_t)-1 on NULL pointer
 * @return the metadata change time
 */
time_t keyGetCTime (const Key * key)
{
	const char * ctime;
	long int val;
	char * endptr;
	int errorval = errno;

	if (!key) return (time_t) -1;

	ctime = keyValue (keyGetMeta (key, "ctime"));
	if (!ctime) return 0;
	if (*ctime == '\0') return (time_t) -1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol (ctime, &endptr, 10);

	/*Check for errors*/
	if (errno) goto cleanup;

	/*Check if nothing was found*/
	if (endptr == ctime) goto cleanup;

	/*Check if the whole string was processed*/
	if (*endptr != '\0') goto cleanup;

	return val;
cleanup:
	/*First restore errno*/
	errno = errorval;
	return (time_t) -1;
}


/**
 * Update the ctime information for a key.
 *
 * @deprecated This API is obsolete.
 *
 * @param key The Key object to work with
 * @param ctime The new change metadata time for the key
 * @retval 0 on success
 * @retval -1 on NULL pointer
 * @see keyGetCTime()
 */
int keySetCTime (Key * key, time_t ctime)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT - 1, "%lu", ctime) < 0)
	{
		return -1;
	}

	keySetMeta (key, "ctime", str);

	return 0;
}

#endif

/**
 * Compare the order metadata of two keys.
 *
 * @return a number less than, equal to or greater than zero if
 *    the order of k1 is found, respectively, to be less than,
 *    to match, or be greater than the order of k2. If one key is
 *    NULL, but the other isn't, the key which is not NULL is considered
 *    to be greater. If both keys are NULL, they are
 *    considered to be equal. If one key does have an order
 *    metadata but the other has not, the key with the metadata
 *    is considered greater. If no key has metadata,
 *    they are considered to be equal.
 *
 * @param ka key to compare with
 * @param kb other key to compare with
 */
int elektraKeyCmpOrder (const Key * ka, const Key * kb)
{

	if (!ka && !kb) return 0;

	if (ka && !kb) return 1;

	if (!ka && kb) return -1;

	int aorder = -1;
	int border = -1;

	const Key * kam = keyGetMeta (ka, "order");
	const Key * kbm = keyGetMeta (kb, "order");

	if (kam) aorder = atoi (keyString (kam));
	if (kbm) border = atoi (keyString (kbm));

	if (aorder > 0 && border > 0) return aorder - border;

	if (aorder < 0 && border < 0) return 0;

	if (aorder < 0 && border >= 0) return -1;

	if (aorder >= 0 && border < 0) return 1;

	/* cannot happen anyway */
	return 0;
}


/**
 * creates an metadata array or appends another element to an existing metadata array
 * e.g.
 * Key *key = keyNew("user/test", KEY_END);
 * elektraMetaArrayAdd(key, "test", "val0");
 * key now has "test/#0" with value "val0" as metadata
 * elektraMetaArrayAdd(key, "test", "val1");
 * appends "test/#1" with value "val1" to key
 *
 * @param key the key the metadata should be added to
 * @param metaName the name of the metakey array parent
 * @param value the value of the newly appended metakey
 */

void elektraMetaArrayAdd (Key * key, const char * metaName, const char * value)
{
	const Key * meta = keyGetMeta (key, metaName);
	Key * arrayKey;
	if (!meta)
	{
		keySetMeta (key, metaName, "#0");
		arrayKey = keyDup (keyGetMeta (key, metaName));
		keySetString (arrayKey, 0);
		keyAddBaseName (arrayKey, "#");
	}
	else
	{
		arrayKey = keyDup (meta);
		keyAddBaseName (arrayKey, keyString (meta));
	}
	elektraArrayIncName (arrayKey);
	keySetMeta (key, keyName (arrayKey), value);
	keySetMeta (key, metaName, keyBaseName (arrayKey));
	keyDel (arrayKey);
}

/**
 * Create a `KeySet` from a metakey array.
 *
 * For example, the following function call
 *
 * @code
elektraMetaArrayToKS(
	keyNew ("/a", KEY_VALUE, "b, c",
		KEY_META, "dep",    "#1",
		KEY_META, "dep/#0", "/b",
		KEY_META, "dep/#1", "/c", KEY_END),
	"dep");
 * @endcode
 *
 * returns a `KeySet` containing the keys `dep` with value `#1`, `"dep/#0"` with value `"/b"` and
 * `"dep/#1"` with value `"/c"`.
 *
 * If no meta key array is found, null is returned.
 * The returned `KeySet` must be freed with `ksDel`
 *
 * @returns a keyset containing all the metakeys of the metakey array
 * @param key the key containing the metakey array
 * @param metaName the name of the metakey array parent
 */
KeySet * elektraMetaArrayToKS (const Key * key, const char * metaName)
{
	const Key * meta = keyGetMeta (key, metaName);
	if (!meta) return NULL;

	KeySet * result = ksNew (0, KS_END);

	if (keyString (meta)[0] != '#')
	{
		ksAppendKey (result, (Key *) meta);
		ksRewind (result);
		return result;
	}
	ksAppendKey (result, keyDup (meta));
	Key * currentKey = keyDup (meta);
	keyAddName (currentKey, "#");
	elektraArrayIncName (currentKey);
	Key * curMeta = NULL;
	while ((curMeta = (Key *) keyGetMeta (key, keyName (currentKey))) != NULL)
	{
		ksAppendKey (result, keyDup (curMeta));
		elektraArrayIncName (currentKey);
	}
	keyDel (currentKey);
	ksRewind (result);
	return result;
}


/**
 * @internal
 *
 * elektraSortTopology helper
 * matrix struct
 */
typedef struct
{
	Key * key;
	kdb_octet_t isResolved;
	unsigned long * deps;
} _adjMatrix;

/**
 * @internal
 *
 * elektraSortTopology helper
 * ordering function for qsort
 */
static int topCmpOrder (const void * a, const void * b)
{
	const Key * ka = (*(const Key **) a);
	const Key * kb = (*(const Key **) b);

	if (!ka && !kb) return 0;
	if (ka && !kb) return 1;
	if (!ka && kb) return -1;

	const Key * kam = keyGetMeta (ka, "order");
	const Key * kbm = keyGetMeta (kb, "order");

	return strcmp (keyString (kam), keyString (kbm));
}


/**
 * @internal
 *
 * elektraSortTopology helper
 * returns the index of dependency depKey
 */
static int getArrayIndex (Key * depKey, _adjMatrix * adjMatrix, size_t size)
{
	for (unsigned int i = 0; i < size; ++i)
	{
		if (!strcmp (keyName (adjMatrix[i].key), keyString (depKey))) return i;
	}
	return -1;
}
/**
 * @internal
 *
 * elektraSortTopology helper
 * removes resolved dependency j from our matrix
 */
static int resolveDep (unsigned int j, _adjMatrix * adjMatrix, size_t size)
{
	int removed = 0;
	for (unsigned int i = 0; i < size; ++i)
	{
		if (adjMatrix[i].deps[j])
		{
			++removed;
			adjMatrix[i].deps[j] = 0;
		}
	}
	return removed;
}

/**
 * @internal
 *
 * elektraSortTopology helper
 * checks if the key with index j has unresolved dependencies
 */
static int hasUnresolvedDependencies (unsigned int j, _adjMatrix * adjMatrix, size_t size)
{
	for (unsigned int i = 0; i < size; ++i)
	{
		if (adjMatrix[j].deps[i]) return 1;
	}
	return 0;
}

/**
 * @internal
 *
 * elektraSortTopology helper
 * resolve all dependencies of the key with the index j in our matrix.
 */
static int resolveDeps (unsigned int j, _adjMatrix * adjMatrix, size_t size, KeySet * done, Key * orderCounter)
{
	unsigned int loops = 0;
	unsigned int frontier[size];
	unsigned int todo = 0;
	for (unsigned int i = 0; i < size; ++i)
	{
		if (adjMatrix[j].deps[i])
		{
			frontier[i] = 1;
			++todo;
		}
		else
		{
			frontier[i] = 0;
		}
	}
	int found = 1;

	// loop until all dependencies are added to frontier
	while (found)
	{
		found = 0;
		for (unsigned int i = 0; i < size; ++i)
		{
			if (!frontier[i]) continue;
			if (hasUnresolvedDependencies (i, adjMatrix, size))
			{
				for (unsigned int k = 0; k < size; ++k)
				{
					if (adjMatrix[i].deps[k])
					{
						if (!frontier[k])
						{
							found = 1;
							++todo;
							frontier[k] = 1;
						}
					}
				}
			}
		}
	}
	if (todo == 0)
	{
		// all dependencies are already resolved, give key an order number and add it to
		// the our list of resolved keys (done)
		adjMatrix[j].isResolved = 1;
		resolveDep (j, adjMatrix, size);
		keySetMeta (adjMatrix[j].key, "order", keyBaseName (orderCounter));
		elektraArrayIncName (orderCounter);
		ksAppendKey (done, keyDup (adjMatrix[j].key));
		return 1;
	}
	unsigned int max_loops = todo;
	for (unsigned int i = 0; todo; ++i)
	{
		if (i == size)
		{
			++loops;
			i = 0;
		}
		if (loops > max_loops) return -1; // more loops than we had unresolved keys -> cycle
		if (!frontier[i]) continue;
		if (!hasUnresolvedDependencies (i, adjMatrix, size))
		{
			resolveDep (i, adjMatrix, size);
			frontier[i] = 0;
			--todo;
			adjMatrix[i].isResolved = 1;
			resolveDep (i, adjMatrix, size);
			keySetMeta (adjMatrix[i].key, "order", keyBaseName (orderCounter));
			elektraArrayIncName (orderCounter);
			ksAppendKey (done, keyDup (adjMatrix[i].key));
		}
	}
	return 1;
}

/**
 * elektraSortTopology helper
 * tests if name is a valid keyname
 */
static int isValidKeyName (const char * testName)
{
	int retVal = 0;
	Key * testKey = keyNew (testName, KEY_CASCADING_NAME, KEY_END);
	if (!strcmp (keyName (testKey), testName)) retVal = 1;
	keyDel (testKey);
	return retVal;
}

/**
 * @brief topological sorting
 *
 * @param array the array where the sorted keys will be stored in topological order.
 *        Nothing will be written into an array if
 * @param ks is the keyset that should be sorted.
 *        Dependencies and order is defined by metakeys.
 *
 * - the "dep/#" metakeys
 *  e.g. the Key *k = keyNew ("/a", KEY_VALUE, "b, c",
 *  KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END), "dep");
 *  depends on Key "/b" and Key "/c".
 * - if "order" metakeys are defined for the keys the algorithm tries to resolves them by that
 *  order using lexical comparison. You should prefer `#0` array syntax.
 *
 * Duplicated and reflexive dep entries are ignored.
 *
 * The algorithm used is a mixture of Kahn and BFS.
 * Furthermore the algorithm does not use recursion.
 *
 * First a BFS with the keys sorted by "order" is used.
 * Then all dependencies (recursively) of every key is collected.
 *
 * @retval 1 on success
 * @retval 0 for cycles
 * @retval -1 for invalid dependencies
 */

int elektraSortTopology (KeySet * ks, Key ** array)
{
	if (ks == NULL || array == NULL) return -1;
	KeySet * done = ksNew (0, KS_END);
	ksRewind (ks);
	Key * cur;
	ssize_t size = ksGetSize (ks);
	Key * orderCounter = keyNew ("/#", KEY_CASCADING_NAME, KEY_END);
	elektraArrayIncName (orderCounter);
	_adjMatrix adjMatrix[size];
	int i = 0;
	int retVal = 1;
	int depCount = 0;
	Key ** localArray = elektraMalloc (size * sizeof (Key *));
	elektraKsToMemArray (ks, localArray);
	qsort (localArray, size, sizeof (Key *), topCmpOrder);
	for (long j = 0; j < size; ++j)
	{
		adjMatrix[j].key = localArray[j];
		adjMatrix[j].isResolved = 0;
		adjMatrix[j].deps = elektraCalloc (sizeof (unsigned long) * size);
	}
	kdb_octet_t hasOrder = 0;
	if (keyGetMeta (localArray[0], "order")) hasOrder = 1;
	unsigned int unresolved = 0;
	for (int j = 0; j < size; ++j)
	{
		cur = localArray[j];
		KeySet * deps = elektraMetaArrayToKS (cur, "dep");
		keyDel (ksLookupByName (deps, "dep", KDB_O_POP));
		Key * tmpDep;
		switch (ksGetSize (deps))
		{
		case -1:
		{
			// key has no dependencies, give it an order number and add it to list of resolved dependencies
			keySetMeta (cur, "order", keyBaseName (orderCounter));
			elektraArrayIncName (orderCounter);
			ksAppendKey (done, keyDup (cur));
			adjMatrix[j].isResolved = 1;
			ksDel (deps);
			break;
		}
		case 1:
		{
			// only 1 dependency:
			// test if it's reflexive
			tmpDep = ksHead (deps);
			if (!strcmp (keyName (cur), keyString (tmpDep)))
			{
				keySetMeta (cur, "order", keyBaseName (orderCounter));
				elektraArrayIncName (orderCounter);
				ksAppendKey (done, keyDup (cur));
				adjMatrix[j].isResolved = 1;
				ksDel (deps);
				break;
			}
			// if not, fallthrough to normal dependency handling
		}
		// FALLTHROUGH
		default:
		{
			int gotUnresolved = 0;
			while ((tmpDep = ksNext (deps)) != NULL)
			{
				if (!isValidKeyName (keyString (tmpDep)))
				{
					// invalid keyname -> ERROR
					retVal = -1;
					break;
				}
				i = getArrayIndex (tmpDep, adjMatrix, size);
				if (i == -1)
				{
					// key doesn't exist yet but has valid name, ignore it.
					continue;
				}
				else if (i == j)
				{
					// reflexiv dependency, do nothing
				}
				else
				{
					if (!adjMatrix[i].isResolved)
					{
						// unresolved dependency
						adjMatrix[j].deps[i] = 1;
						++gotUnresolved;
						// simple cycle detection
						if (adjMatrix[i].deps[j])
						{
							retVal = 0;
							break;
						}
					}
				}
			}
			if (gotUnresolved)
			{
				adjMatrix[j].isResolved = 0;
				++unresolved;
				// count unresolved dependencies
				depCount += gotUnresolved;
			}
			ksDel (deps);
			break;
		}
		}
		if (retVal <= 0) break;
	}
	if (retVal <= 0)
	{
		// error or cycle: goto cleanup
		goto TopSortCleanup;
	}

	// resolve all dependencies that can be resolved immediately
	for (int j = 0; j < size; ++j)
	{
		if (adjMatrix[j].isResolved) depCount -= resolveDep (j, adjMatrix, size);
	}

	ssize_t resolved = ksGetSize (done);
	if (((depCount + resolved) >= size) && (unresolved))
	{
		// more dependencies dependencies than keys:
		//  cycle found !
		retVal = 0;
		goto TopSortCleanup;
	}

	if (unresolved)
	{
		int found = 1;
		// we have unresolved dependencies
		for (int j = 0; j < size + 1; ++j)
		{
			// loop until no dependency can be resolved anymore
			if (j == size)
			{
				if (found)
				{
					found = 0;
					j = -1;
					unresolved = 0;
					continue;
				}
				else
					break;
			}
			if (adjMatrix[j].isResolved) continue;
			++unresolved;
			if (hasOrder)
			{
				// resolve by order
				int ret = resolveDeps (j, adjMatrix, size, done, orderCounter);
				if (ret == -1) break;
				j = -1;
				found = 1;
				continue;
			}
			else
			{
				// resolve next possible dependency in keyset
				if (!hasUnresolvedDependencies (j, adjMatrix, size))
				{
					adjMatrix[j].isResolved = 1;
					resolveDep (j, adjMatrix, size);
					keySetMeta (localArray[j], "order", keyBaseName (orderCounter));
					elektraArrayIncName (orderCounter);
					ksAppendKey (done, keyDup (localArray[j]));
					found = 1;
				}
			}
		}
	}
	if (unresolved == 0)
	{
		// everything resolved
		// add dependencies in topological order to array
		elektraKsToMemArray (ks, array);
		qsort (array, size, sizeof (Key *), topCmpOrder);
		retVal = 1;
	}
	else
	{
		// still unresolved dependencies left:
		// there must be a cycle somewhere
		retVal = 0;
	}
TopSortCleanup:
	ksDel (done);
	keyDel (orderCounter);
	elektraFree (localArray);
	for (ssize_t j = 0; j < size; ++j)
	{
		elektraFree (adjMatrix[j].deps);
	}
	return retVal;
}

/**
 * returns the metakey array as a string separated by delim
 *
 * @param key the key containing the metakey array
 * @param metaName the name of the metakey array parent
 * @param delim delimiter for the records in the returned string
 *
 * @returns a string containing all metakey values separated by "delim"
 */

char * elektraMetaArrayToString (const Key * key, const char * metaName, const char * delim)
{
	char * result = NULL;
	Key * lookupElem = keyDup (keyGetMeta (key, metaName));
	keyAddBaseName (lookupElem, "#0");
	Key * elem = (Key *) keyGetMeta (key, keyName (lookupElem));
	if (elem != NULL)
	{
		elektraRealloc ((void **) &result, keyGetValueSize (elem));
		snprintf (result, keyGetValueSize (elem), "%s", keyString (elem));
	}
	elektraArrayIncName (lookupElem);
	elem = (Key *) keyGetMeta (key, keyName (lookupElem));
	while (elem != NULL)
	{
		elektraRealloc ((void **) &result,
				elektraStrLen (result) + keyGetValueSize (elem) + 1); // String (incl. +2 times \0) + delimiter + whitespace
		strcat (result, delim);
		strcat (result, keyString (elem));
		elektraArrayIncName (lookupElem);
		elem = (Key *) keyGetMeta (key, keyName (lookupElem));
	}
	keyDel (lookupElem);
	return result;
}

/**
 * @}
 */

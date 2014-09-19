 /***************************************************************************
                      keymeta.c  -  Methods for Key manipulation
                             -------------------
    begin                : Fri Sep 26 2008
    copyright            : (C) 2008 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/**
 * @defgroup keymeta Meta Info Manipulation Methods
 * @ingroup key
 * @brief Methods to do various operations on Key metainfo
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * Next to \link keyname Name (key and owner) \endlink and 
 * \link keyvalue Value (data and comment) \endlink there
 * is the so called meta information inside every key.
 *
 * Key meta information are an unlimited number of key/value
 * pairs strongly related to a key. It main purpose is to
 * give keys special semantics, so that plugins can treat
 * them differently.
 *
 * Metakey, as opposed to Key,
 * deliberately has following limitations:
 * - no null values
 * - no binary data
 * - no modification of references (COW)
 * - no guarantee of ordering
 *
 * ## Examples for metadata
 *
 * File system information (see stat(2) for more information):
 * - uid: the user id (positive number)
 * - gid: the group id (positive number)
 * - mode: filesystem-like mode permissions (positive octal number)
 * - atime: When was the key accessed the last time.
 * - mtime: When was the key modified the last time.
 * - ctime: When the uid, gid or mode of a key changes.
 * (times are represented through a positive number as unix timestamp)
 *
 * The comment can contain userdata which directly
 * belong to that key. The name of the meta information
 * is "comment" for a general purpose comment about
 * the key. Multi-Language comments are also supported
 * by appending [LANG] to the name.
 *
 * Validators are regular expressions which are tested
 * against the key value. The metakey "validator" can
 * hold a regular expression which will be matched
 * against.
 *
 * Types can be expressed with the meta information
 * "type".
 *
 * The relevance of the key can be tagged with a value
 * from -20 to 20. Negative numbers are the more important
 * and must be present in order to start the program.
 *
 * A version of a key may be stored with "version".
 * Its format is full.major.minor where all of these
 * are integers.
 *
 * The order inside a persistent storage can be described
 * with the tag "order" which contains a positive number.
 *
 * The meta key "app" describes to which application a
 * key belongs. It can be used to remove keys from an
 * application no longer installed.
 *
 * The meta key "path" describes where the key is physically
 * stored.
 *
 * The "owner" is the user that owns the key. It only
 * works for the user/ hierarchy. It rather says where
 * the key is stored and says nothing about the
 * filesystem properties.
 *
 */



#include <kdb.h>
#include <kdbconfig.h>
#include <kdbprivate.h>

#if HAVE_STDIO_H
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


/**Rewind the internal iterator to first meta data.
 *
 * Use it to set the cursor to the beginning of the Key Meta Infos.
 * keyCurrentMeta() will then always return NULL afterwards. So
 * you want to keyNextMeta() first.
 *
 * @code
Key *key;
const Key *meta;

keyRewindMeta (key);
while ((meta = keyNextMeta (key))!=0)
{
	printf ("name: %s, value: %s", keyName(meta), (const char*)keyValue(meta));
}
 * @endcode
 *
 * @param key the key object to work with
 * @return 0 on success
 * @return 0 if there is no meta information for that key
 *         (keyNextMeta() will always return 0 in that case)
 * @return -1 on NULL pointer
 * @see keyNextMeta(), keyCurrentMeta()
 * @see ksRewind() for pedant in iterator interface of KeySet
 * @ingroup keymeta
 **/
int keyRewindMeta(Key *key)
{
	if (!key) return -1;
	if (!key->meta) return 0;

	return ksRewind(key->meta);
}

/** Iterate to the next meta information.
 *
 * Keys have an internal cursor that can be reset with keyRewindMeta(). Every
 * time keyNextMeta() is called the cursor is incremented and the new current
 * Name of Meta Information is returned.
 *
 * You'll get a NULL pointer if the meta information after the end of the Key was reached.
 * On subsequent calls of keyNextMeta() it will still return the NULL pointer.
 *
 * The @p key internal cursor will be changed, so it is not const.
 *
 * @note That the resulting key is guaranteed to have a value, because
 *       meta information has no binary or null pointer semantics.
 *
 * @note You must not delete or change the returned key,
 *    use keySetMeta() if you want to delete or change it.
 *
 * @param key the key object to work with
 * @return a key representing meta information
 * @return 0 when the end is reached
 * @return 0 on NULL pointer
 *
 * @see ksNext() for pedant in iterator interface of KeySet
 * @ingroup keymeta
  **/
const Key *keyNextMeta(Key *key)
{
	Key *ret;
	if (!key) return 0;
	if (!key->meta) return 0;

	ret = ksNext(key->meta);

	return ret;
}

/**Returns the Value of a Meta-Information which is current.
 *
 * The pointer is NULL if you reached the end or after
 * ksRewind().
 *
 * @note You must not delete or change the returned key,
 *    use keySetMeta() if you want to delete or change it.
 *
 * @param key the key object to work with
 * @return a buffer to the value pointed by @p key's cursor
 * @return 0 on NULL pointer
 * @see keyNextMeta(), keyRewindMeta()
 *
 * @see ksCurrent() for pedant in iterator interface of KeySet
 * @ingroup keymeta
 **/
const Key *keyCurrentMeta(const Key *key)
{
	Key *ret;
	if (!key) return 0;
	if (!key->meta) return 0;

	ret = ksCurrent(key->meta);

	return ret;
}

/**Do a shallow copy of meta data from source to dest.
 *
 * The key dest will have the same meta data referred with
 * metaName afterwards then source.
 *
 * For example the meta data type is copied into the
 * Key k.
 *
 * @code
void l(Key *k)
{
	// receive c
	keyCopyMeta(k, c, "type");
	// the caller will see the changed key k
	// with the metadata "type" from c
}
 * @endcode
 *
 * The main purpose of this function is for plugins or
 * applications which want to add the same meta data to
 * n keys. When you do that with keySetMeta() it will
 * take n times the memory for the key. This can be
 * considerable amount of memory for many keys with
 * some meta data for each.
 *
 * To avoid that problem you can use keyCopyAllMeta()
 * or keyCopyMeta().
 *
 * @code
void o(KeySet *ks)
{
	Key *current;
	Key *shared = keyNew (0);
	keySetMeta(shared, "shared", "this meta data should be shared among many keys");

	ksRewind(ks);
	while ((current = ksNext(ks)) != 0)
	{
		if (needs_shared_data(current)) keyCopyMeta(current, shared, "shared");
	}
}
 * @endcode
 *
 * @post keyGetMeta(source, metaName) == keyGetMeta(dest, metaName)
 *
 * @return 1 if was successfully copied
 * @return 0 if the meta data in dest was removed too
 * @return -1 on null pointers (source or dest)
 * @return -1 on memory problems
 * @param dest the destination where the meta data should be copied too
 * @param source the key where the meta data should be copied from
 * @param metaName the name of the meta data which should be copied
 * @ingroup keymeta
 */
int keyCopyMeta(Key *dest, const Key *source, const char *metaName)
{
	Key *ret;

	if (!source) return -1;
	if (!dest) return -1;
	if (dest->flags & KEY_FLAG_RO_META) return -1;

	ret = (Key*) keyGetMeta (source, metaName);

	if (!ret)
	{
		/*Make sure that dest also does not have metaName*/
		if (dest->meta)
		{
			Key *r;
			r = ksLookup(dest->meta, ret, KDB_O_POP);
			if (r)
			{
				/*It was already there, so lets drop that one*/
				keyDel(r);
			}
		}
		return 0;
	}

	/*Lets have a look if the key is already inserted.*/
	if (dest->meta)
	{
		Key *r;
		r = ksLookup(dest->meta, ret, KDB_O_POP);
		if (r)
		{
			/*It was already there, so lets drop that one*/
			keyDel(r);
		}
	} else {
		/*Create a new place for meta information.*/
		dest->meta = ksNew(0, KS_END);
		if (!dest->meta)
		{
			return -1;
		}
	}

	// now we can simply append that key
	ksAppendKey (dest->meta, ret);

	return 1;
}

/**Do a shallow copy of all meta data from source to dest.
 *
 * The key dest will additionally have all meta data
 * source had.
 * Meta data not present in source will not be changed.
 * Meta data which was present in source and dest will
 * be overwritten.
 *
 * For example the meta data type is copied into the
 * Key k.
 *
 * @code
void l(Key *k)
{
	// receive c
	keyCopyMeta(k, c);
	// the caller will see the changed key k
	// with all the metadata from c
}
 * @endcode
 *
 * The main purpose of this function is for plugins or
 * applications which want to add the same meta data to
 * n keys. When you do that with keySetMeta() it will
 * take n times the memory for the key. This can be
 * considerable amount of memory for many keys with
 * some meta data for each.
 *
 * To avoid that problem you can use keyCopyAllMeta()
 * or keyCopyMeta().
 *
 * @code
void o(KeySet *ks)
{
	Key *current;
	Key *shared = keyNew (0);
	keySetMeta(shared, "shared1", "this meta data should be shared among many keys");
	keySetMeta(shared, "shared2", "this meta data should be shared among many keys also");
	keySetMeta(shared, "shared3", "this meta data should be shared among many keys too");

	ksRewind(ks);
	while ((current = ksNext(ks)) != 0)
	{
		if (needs_shared_data(current)) keyCopyAllMeta(current, shared);
	}
}
 * @endcode
 *
 * @post for every metaName present in source: keyGetMeta(source, metaName) == keyGetMeta(dest, metaName)
 *
 * @return 1 if was successfully copied
 * @return 0 if source did not have any meta data
 * @return -1 on null pointers (source or dest)
 * @return -1 on memory problems
 * @param dest the destination where the meta data should be copied too
 * @param source the key where the meta data should be copied from
 * @ingroup keymeta
 */
int keyCopyAllMeta(Key *dest, const Key *source)
{
	if (!source) return -1;
	if (!dest) return -1;
	if (dest->flags & KEY_FLAG_RO_META) return -1;


	if (source->meta)
	{
		/*Make sure that dest also does not have metaName*/
		if (dest->meta)
		{
			ksAppend (dest->meta, source->meta);
		} else {
			dest->meta = ksDup (source->meta);
		}
		return 1;
	}

	return 0;
}

/**Returns the Value of a Meta-Information given by name.
 *
 * This is a much more efficient version of keyGetMeta().
 * But unlike with keyGetMeta you are not allowed to modify
 * the resulting string.
 *
 * @code
int f(Key *k)
{
	if (!strcmp(keyValue(keyGetMeta(k, "type")), "boolean"))
	{
		// the type of the key is boolean
	}
}
 * @endcode
 *
 * @note You must not delete or change the returned key,
 *    use keySetMeta() if you want to delete or change it.
 *
 * @param key the key object to work with
 * @param metaName the name of the meta information you want the value from
 * @return 0 if the key or metaName is 0
 * @return 0 if no such metaName is found
 * @return value of Meta-Information if Meta-Information is found
 * @see keyGetMeta(), keySetMeta()
 * @ingroup keymeta
 **/
const Key *keyGetMeta(const Key *key, const char* metaName)
{
	Key *ret;
	Key *search;

	if (!key) return 0;
	if (!metaName) return 0;
	if (!key->meta) return 0;

	search = keyNew (0);
	elektraKeySetName(search, metaName, KDB_O_META_NAME | KDB_O_EMPTY_NAME);

	ret = ksLookup(key->meta, search, 0);

	keyDel (search);

	return ret;
}


/**Set a new Meta-Information.
 *
 * Will set a new Meta-Information pair consisting of
 * metaName and newMetaString.
 *
 * Will add a new Pair for Meta-Information if metaName was
 * not added up to now.
 *
 * It will modify a existing Pair of Meta-Information if the
 * the metaName was inserted already.
 *
 * It will remove a meta information if newMetaString is 0.
 *
 * @param key the key object to work with
 * @param metaName the name of the meta information where you
 *                 want to change the value
 * @param newMetaString the new value for the meta information
 * @return -1 on error if key or metaName is 0, out of memory
 *         or names are not valid
 * @return 0 if the Meta-Information for metaName was removed
 * @return size (>0) of newMetaString if Meta-Information was
 *         successfully added
 * @see keyGetMeta()
 * @ingroup keymeta
 **/
ssize_t keySetMeta(Key *key, const char* metaName,
	const char *newMetaString)
{
	Key *toSet;
	char *metaStringDup;
	ssize_t metaNameSize;
	ssize_t metaStringSize = 0;

	if (!key) return -1;
	if (key->flags & KEY_FLAG_RO_META) return -1;
	if (!metaName) return -1;
	metaNameSize = elektraStrLen (metaName);
	if (metaNameSize == -1) return -1;
	if (newMetaString) metaStringSize = elektraStrLen (newMetaString);

	toSet = keyNew(0);
	if (!toSet) return -1;

	elektraKeySetName(toSet, metaName, KDB_O_META_NAME | KDB_O_EMPTY_NAME);

	/*Lets have a look if the key is already inserted.*/
	if (key->meta)
	{
		Key *ret;
		ret = ksLookup(key->meta, toSet, KDB_O_POP);
		if (ret)
		{
			/*It was already there, so lets drop that one*/
			keyDel (ret);
		}
	}

	if (newMetaString)
	{
		/*Add the meta information to the key*/
		metaStringDup = elektraStrNDup(newMetaString, metaStringSize);
		if (!metaStringDup)
		{
			keyDel (toSet);
			return -1;
		}

		if (toSet->data.v) free (toSet->data.v);
		toSet->data.c = metaStringDup;
		toSet->dataSize = metaStringSize;
	} else {
		/*The request is to remove the meta string.
		  So simply drop it.*/
		keyDel (toSet);
		return 0;
	}

	if (!key->meta)
	{
		/*Create a new place for meta information.*/
		key->meta = ksNew(0, KS_END);
		if (!key->meta)
		{
			keyDel (toSet);
			return -1;
		}
	}

	set_bit(toSet->flags, KEY_FLAG_RO_NAME);
	set_bit(toSet->flags, KEY_FLAG_RO_VALUE);
	set_bit(toSet->flags, KEY_FLAG_RO_META);

	ksAppendKey (key->meta, toSet);
	key->flags |= KEY_FLAG_SYNC;
	return metaStringSize;
}



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
 * @return (uid_t)-1 on NULL key
 * @see keyGetGID(), keySetUID(), keyGetOwner()
 * @ingroup keymeta
 */
uid_t keyGetUID(const Key *key)
{
	const char *uid;
	long int val;
	char *endptr;
	int errorval = errno;

	if (!key) return (uid_t)-1;

	uid = keyValue(keyGetMeta(key, "uid"));
	if (!uid) return (uid_t)-1;
	if (*uid == '\0') return (uid_t)-1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol(uid, &endptr, 10);

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
	return (uid_t)-1;
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
 * @return 0 on success
 * @return -1 on NULL key or conversion error
 * @see keySetGID(), keyGetUID(), keyGetOwner()
 * @ingroup keymeta
 */
int keySetUID(Key *key, uid_t uid)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT-1, "%d", uid) < 0)
	{
		return -1;
	}

	keySetMeta(key, "uid", str);

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
 * @return (gid_t)-1 on NULL key or currently unknown ID
 * @see keySetGID(), keyGetUID()
 * @ingroup keymeta
 */
gid_t keyGetGID(const Key *key)
{
	const char *gid;
	long int val;
	char *endptr;
	int errorval = errno;

	if (!key) return (gid_t)-1;

	gid = keyValue(keyGetMeta(key, "gid"));
	if (!gid) return (gid_t)-1;
	if (*gid == '\0') return (gid_t)-1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol(gid, &endptr, 10);

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
	return (gid_t)-1;
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
 * @return 0 on success
 * @return -1 on NULL key
 * @see keyGetGID(), keySetUID()
 * @ingroup keymeta
 */
int keySetGID(Key *key, gid_t gid)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT-1, "%d", gid) < 0)
	{
		return -1;
	}

	keySetMeta(key, "gid", str);

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
 * @return 0 on success
 * @return -1 on NULL pointer
 * @see keySetMode()
 * @ingroup keymeta
 */
int keySetDir(Key *key)
{
	mode_t mode;
	if (!key) return -1;

	mode = keyGetMode(key);
	mode |= KDB_DIR_MODE;
	keySetMode(key, mode);

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
 * @return KDB_FILE_MODE as defaults
 * @return (mode_t)-1 on NULL pointer
 * @see keySetMode()
 * @ingroup keymeta
 */
mode_t keyGetMode(const Key *key)
{
	const char *mode;
	long int val;
	char *endptr;
	int errorval = errno;

	if (!key) return (mode_t)-1;

	mode = keyValue(keyGetMeta(key, "mode"));
	if (!mode) return KDB_FILE_MODE;
	if (*mode == '\0') return KDB_FILE_MODE;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol(mode, &endptr, 8);

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
 * to keySetMeta(key, "mode", str) which should be prefered.
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
 * for that user, but not accessable. 0200 only allows reading.
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
 * account when neighter the uid nor the gid matches. So that
 * example would allow everyone with a different uid and gid
 * of that key gains full access.
 *
 * @param key the key to set mode permissions
 * @param mode the mode permissions
 * @return 0 on success
 * @return -1 on NULL key
 * @see keyGetMode()
 * @ingroup keymeta
 */
int keySetMode(Key *key, mode_t mode)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT-1, "%o", mode) < 0)
	{
		return -1;
	}

	keySetMeta(key, "mode", str);

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
 * @return 0 on key that was never kdbGet()
 * @return (time_t)-1 on NULL pointer
 * @see keySetATime()
 * @see kdbGet()
 * @ingroup keymeta
 */
time_t keyGetATime(const Key *key)
{
	const char *atime;
	long int val;
	char *endptr;
	int errorval = errno;

	if (!key) return (time_t)-1;

	atime = keyValue(keyGetMeta(key, "atime"));
	if (!atime) return 0;
	if (*atime == '\0') return (time_t)-1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol(atime, &endptr, 10);

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
	return (time_t)-1;
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
 * @return 0 on success
 * @return -1 on NULL pointer
 * @see keyGetATime()
 * @ingroup keymeta
 */
int keySetATime(Key *key, time_t atime)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT-1, "%lu", atime) < 0)
	{
		return -1;
	}

	keySetMeta(key, "atime", str);

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
 * @return (time_t)-1 on NULL pointer
 * @ingroup keymeta
 */
time_t keyGetMTime(const Key *key)
{
	const char *mtime;
	long int val;
	char *endptr;
	int errorval = errno;

	if (!key) return (time_t)-1;

	mtime = keyValue(keyGetMeta(key, "mtime"));
	if (!mtime) return 0;
	if (*mtime == '\0') return (time_t)-1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol(mtime, &endptr, 10);

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
	return (time_t)-1;
}

/**
 * Update the mtime information for a key.
 *
 * @deprecated This API is obsolete.
 *
 * @param key The Key object to work with
 * @param mtime The new modification time for the key
 * @return 0 on success
 * @see keyGetMTime()
 * @ingroup keymeta
 */
int keySetMTime(Key *key, time_t mtime)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT-1, "%lu", mtime) < 0)
	{
		return -1;
	}

	keySetMeta(key, "mtime", str);

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
 * @return (time_t)-1 on NULL pointer
 * @return the metadata change time
 * @ingroup keymeta
 */
time_t keyGetCTime(const Key *key)
{
	const char *ctime;
	long int val;
	char *endptr;
	int errorval = errno;

	if (!key) return (time_t)-1;

	ctime = keyValue(keyGetMeta(key, "ctime"));
	if (!ctime) return 0;
	if (*ctime == '\0') return (time_t)-1;

	/*From now on we have to leave using cleanup*/
	errno = 0;
	val = strtol(ctime, &endptr, 10);

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
	return (time_t)-1;
}


/**
 * Update the ctime information for a key.
 *
 * @deprecated This API is obsolete.
 *
 * @param key The Key object to work with
 * @param ctime The new change metadata time for the key
 * @return 0 on success
 * @return -1 on NULL pointer
 * @see keyGetCTime()
 * @ingroup keymeta
 */
int keySetCTime(Key *key, time_t ctime)
{
	char str[MAX_LEN_INT];
	if (!key) return -1;

	if (snprintf (str, MAX_LEN_INT-1, "%lu", ctime) < 0)
	{
		return -1;
	}

	keySetMeta(key, "ctime", str);

	return 0;
}



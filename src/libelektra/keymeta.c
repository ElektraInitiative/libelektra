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
 * @defgroup keymeta Key :: Meta Info Manipulation Methods
 * @brief Methods to do various operations on Key metainfo
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * Next to \link keyname Name (key and owner) \endlink and 
 * \link keyvalue Value (data and comment) \endlink there
 * is the so called metainfo inside every key.
 *
 * Key metainfo insists of:
 * - UID, the user id
 * - GID, the group id 
 * - filesystem-like mode permissions (rwx)
 * - Mode, change and modification times
 *
 * The comment can contain userdata which directly
 * belong to that key.
 *
 * Owner is the user that owns the key. It only
 * works for the user/ hierachy.
 *
 * Every user and group of your System has a uniqe ID.
 * These values are used in the keys too. They are
 * very important for the mode. See man 2 chown.
 *
 * With the mode mode you can choose if a user, group
 * or the world can mode your key. See man 2 chmod.
 *
 */



#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if DEBUG && HAVE_STDIO_H
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


#include "kdb.h"
#include "kdbprivate.h"

/**Rewind the internal iterator to first meta data.
 *
 * Use it to set the cursor to the beginning of the Key Meta Infos.
 * keyCurrentMeta() will then always return NULL afterwards. So
 * you want to keyNextMeta() first.
 *
 * @code
Key *key;
const char *name;

keyRewindMeta (key);
while ((name = keyNextMeta (key))!=0) {}
 * @endcode
 *
 * @param key the key object to work with
 * @return 0 on success
 * @return 0 if there is no meta information for that key
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
 * @note You must not delete or change the returned buffer,
 *       use keySetMeta() if you want to delete/change it.
 *
 * @param key the key object to work with
 * @return a buffer to the name of the key's meta info
 * @return 0 when the end is reached
 * @return 0 on NULL pointer
  *
  * @see ksNext() for pedant in iterator interface of KeySet
 * @ingroup keymeta
  **/
const char *keyNextMeta(Key *key)
{
	Key *ret;
	if (!key) return 0;
	if (!key->meta) return 0;

	ret = ksNext(key->meta);

	if (!ret) return 0;
	return keyName(ret);
}

/**Returns the Value of a Meta-Information which is current.
 *
 * The pointer is NULL if you reached the end or after
 * ksRewind().
 *
 * @note You must not delete or change the returned buffer,
 *    use keySetMeta() if you want to delete or change it.
 *
 * @param ks the keyset object to work with
 * @return a buffer to the value pointed by @p key's cursor
 * @return 0 on NULL pointer
 * @see keyNextMeta(), keyRewindMeta()
 *
 * @see ksCurrent() for pedant in iterator interface of KeySet
 * @ingroup keymeta
  **/
const char *keyCurrentMeta(const Key *key)
{
	Key *ret;
	if (!key) return 0;
	if (!key->meta) return 0;

	ret = ksCurrent(key->meta);

	if (!ret) return 0;
	return keyValue(ret);
}

/*Returns the key where a meta value is stored.
  This should not passed to the user because it
  is an implementation detail.*/
static inline Key *keyMetaKey(const Key* key, const char* metaName)
{
	Key *ret;
	Key *search;

	if (!key) return 0;
	if (!metaName) return 0;
	if (!key->meta) return 0;

	search = keyNew (KEY_END);
	search->key = kdbiStrDup(metaName);

	if (!search->key) return 0; /*Duplication did not work*/

	ret = ksLookup(key->meta, search, 0);

	keyDel (search);

	return ret;
}

/**Returns the Value of a Meta-Information given by name.
 *
 * This is a much more efficient version of keyGetMeta().
 * But unlike with keyGetMeta you are not allowed to modify
 * the resulting string.
 *
 * @code
 * @endcode
 *
 * keyMeta() can be used to get the value of the current value,
 * see iterator keyNextMeta(), keyCurrentMeta()
 *
 * @return 0 if the key or metaName is 0
 * @return 0 if no such metaName is found
 * @return value of Meta-Information if Meta-Information is found
 * @see keyGetMetaSize(), keyGetMeta(), keySetMeta()
 * @ingroup keymeta
 **/
const char *keyMeta(const Key *key, const char* metaName)
{
	Key *ret = keyMetaKey (key, metaName);
	return keyValue(ret);
}

/**
 * Returns the number of bytes needed to store the key meta value, including the
 * NULL terminator.
 *
 * It returns the correct size.
 *
 * For an empty string you need one byte to store the ending NULL.
 * For that reason 1 is returned. This is not true for binary data,
 * so there might be returned 0 too.
 *
 * A binary key has no '\\0' termination. String types have it, so to there
 * length will be added 1 to have enough space to store it.
 *
 * This method can be used with malloc() before keyGetString() or keyGetBinary()
 * is called.
 *
 * @code
char *buffer;
buffer = malloc (keyGetValueSize (key));
// use this buffer to store the value (binary or string)
// pass keyGetValueSize (key) for maxSize
 * @endcode
 *
 * @param key the key object to work with
 * @return the number of bytes needed to store the key value
 * @return 1 when there is an empty meta value
 * @return 0 when there is no meta value (deleted or never added)
 * @return -1 on null pointer
 * @see keyGetString(), keyGetBinary(), keyValue()
 * @ingroup keymeta
 */
ssize_t keyGetMetaSize(const Key *key, const char* metaName)
{
	Key *ret;

	if (!key) return -1;

	ret = keyMetaKey (key, metaName);
	if (!ret) return 0;
	return keyGetValueSize(ret);
}

/**
 * Get the value of a key as a string.
 *
 * When there is no value inside the string, 1 will
 * be returned and the returnedString will be empty
 * "" to avoid programming errors that old strings are
 * shown to the user.
 *
 * For binary values see keyGetBinary() and keyIsBinary().
 *
 * @par Example:
 * @code
Key *key = keyNew ("user/keyname", KEY_END);
char buffer[300];

if (keyGetString(key,buffer,sizeof(buffer)) == -1)
{
	// handle error
} else {
	printf ("buffer: %s\n", buffer);
}
 * @endcode
 *
 * @param key the object to gather the value from
 * @param returnedString pre-allocated memory to store a copy of the key value
 * @param maxSize number of bytes of allocated memory in @p returnedString
 * @return the number of bytes actually copied to @p returnedString, including
 * 	final NULL
 * @return 1 if the string is empty
 * @return -1 on NULL pointer
 * @return -1 on type mismatch
 * @return maxSize is 0, too small for string or is larger than SSIZE_MAX
 * @see keyValue(), keyGetValueSize(), keySetString()
 * @see keyGetBinary() for working with binary data
 * @ingroup keymeta
 */
ssize_t keyGetMeta(const Key *key, const char* metaName,
	char *returnedMetaString, size_t maxSize)
{
	Key *ret = keyMetaKey (key, metaName);
	if (!ret)
	{
		return 0;
	}
	return keyGetString (ret, returnedMetaString, maxSize);
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
 * @ingroup keymeta
 * @return -1 on error if key or metaName is 0, out of memory
 *         or names are not valid
 * @return 0 if the Meta-Information for metaName was removed
 * @return size (>0) of newMetaString if Meta-Information was
 *         successfully added
 * @see keyMeta(), keyGetMetaSize(), keyGetMeta()
 **/
ssize_t keySetMeta(Key *key, const char* metaName,
	const char *newMetaString)
{
	Key *toSet;
	char *metaNameDup;
	char *metaStringDup;
	ssize_t metaNameSize;
	ssize_t metaStringSize;

	if (!key) return -1;
	if (!metaName) return -1;
	metaNameSize = kdbiStrLen (metaName);
	if (metaNameSize == -1) return -1;
	if (newMetaString)
	{
		metaStringSize = kdbiStrLen (newMetaString);
		if (metaStringSize == -1) return -1;
	}

	toSet = keyNew(KEY_END);
	if (!toSet) return -1;

	metaNameDup = kdbiStrNDup(metaName, metaNameSize);
	if (!metaNameDup)
	{
		keyDel (toSet);
		return -1;
	}
	toSet->key = metaNameDup;
	toSet->keySize = metaNameSize;
	toSet->type = KEY_TYPE_STRING;

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
		metaStringDup = kdbiStrNDup(newMetaString, metaStringSize);
		if (!metaStringDup)
		{
			keyDel (toSet);
			return -1;
		}

		if (toSet->data) free (toSet->data);
		toSet->data = metaStringDup;
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
		key->meta = ksNew(0);
		if (!key->meta)
		{
			keyDel (toSet);
			return -1;
		}
	}

	ksAppendKey (key->meta, toSet);
	key->flags |= KEY_FLAG_SYNC;
	return metaStringSize;
}


/**
 * Only stat a key instead of receiving value, comment and key type.
 *
 * Only stat the key in the database when doing
 * kdbGet(). The key may not have any value, comment
 * or key type set.
 *
 * It is not possible to revert the action on per-key basis.
 * When you want to remove the flag you have to pass
 * option_t::KDB_O_NOSTAT to the next kdbGet().
 *
 * @see keyNeedStat(), kdbGet()
 * @param key the key object to work with
 * @return 1 on succuess
 * @return -1 on NULL pointer
 *
 * @ingroup keymeta
 */
int keyStat(Key *key)
{
	if (!key) return -1;

	key->flags |= KEY_FLAG_STAT;
	return 1;
}


/**
 * Permanently remove a key after committing to database.
 *
 * This functions sets a flag that the key needs to be removed.
 * It also sets a flag that it is not synced.
 *
 * Remove the key instead of writing it in the key database when doing
 * kdbSet() and related functions.
 *
 * This key will be ignored and it is save to delete it afterwards.
 * To be sure that it was removed, check if it needs sync with
 * keyNeedSync().
 *
 * @note Delete in elektra terminology means to free memory,
 * remove means to free permanent storage.
 *
 * @warning You should not change a key's remove status once it belongs to a keyset.
 * See ksSort() for more information.
 *
 * @see keyNeedRemove(), kdbSet(), kdbRemove()
 * @param key the key object to work with
 * @return 1 on success
 * @return -1 on NULL pointer
 *
 * @ingroup keymeta
 */
int keyRemove(Key *key) {
	if (!key) return -1;

	key->flags |= KEY_FLAG_REMOVE;
	key->flags |= KEY_FLAG_SYNC;
	return 1;
}


/*********************************************
 *       UID, GID access methods             *
 *********************************************/


/**The maximum of how many characters an integer
  needs as decimal number.*/
#define MAX_LEN_INT 31
#include <errno.h>



/**
 * Get the user ID of a key.
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

	uid = keyMeta (key, "uid");
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

	gid = keyMeta (key, "gid");
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
 * The function will add all executable bits.
 *
 * - Mode 0200 will be translated to 0311
 * - Mode 0400 will be translated to 0711
 * - Mode 0664 will be translated to 0775
 *
 * The macro KEY_DEF_DIR (defined to 0111) will be used for that.
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
	if (!key) return -1;

	key->mode |= KEY_DEF_DIR;
	key->flags |= KEY_FLAG_SYNC;
	
	return 0;
}



/**
 * Return the key mode permissions.
 *
 * Default is 0664 (octal) for keys and 0775 for directory keys
 * which used keySetDir().
 *
 * The defaults are defined with the macros KEY_DEF_MODE and KEY_DEF_DIR.
 *
 * For more information about the mode permissions see @ref mode.
 *
 * @param key the key object to work with
 * @return mode permissions of the key
 * @return (mode_t)-1 on NULL pointer
 * @see keySetMode()
 * @ingroup keymeta
 */
mode_t keyGetMode(const Key *key)
{
	if (!key) return (mode_t) -1;

	return key->mode;
}



/**
 * Set the key mode permissions.
 *
 * The mode consists of 9 individual bits for mode permissions.
 * In the following explanation the octal notation with leading
 * zero will be used.
 *
 * Default is 0664 (octal) for keys and 0775 for directory keys
 * which used keySetDir().
 *
 * The defaults are defined with the macros KEY_DEF_MODE and KEY_DEF_DIR.
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
 * Reading the key means to get the value and comment by kdbGet()
 * or all highlevel methods.
 *
 * Writing the key means to set the value and comment by kdbSet()
 * or all highlevel methods.
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
	if (!key) return -1;

	key->mode=mode;
	key->flags |= KEY_FLAG_SYNC;

	return 0;
}



/**
 * Returns the key data type.
 *
 * See #type_t for the type definition.
 *
 * @see keySetType()
 * @see keyIsBinary() and keyIsString()
 * @see keyIsDir() is not related to the type system
 * @param key key where to get the type.
 * @return the key type
 * @return KEY_TYPE_UNDEFINED on keys without type
 * @return -1 on NULL pointer
 * @ingroup keymeta
 *
 */
type_t keyGetType(const Key *key)
{
	if (!key) return -1;

	return key->type;
}



/**
 * Set a new key type.
 *
 * This method is usually not needed, unless you are working with more
 * semantic value types, or want to force a specific value type for a key.
 * It is not usually needed because the data type is automatically set
 * when setting the key value.
 *
 * See #type_t for the type defintion.
 *
 * @par Example:
 * @code
// here we define the new type
enum
{
	KEY_TYPE_COLOR=KEY_TYPE_STRING+4
};
// here we make a new key with the type
Key *k1 = keyNew ("user/sw/oyranos/current/color1",
	KEY_VALUE, "#4B52CA",
	KEY_COMMENT, "a custom color",
	KEY_TYPE, KEY_TYPE_COLOR,
	KEY_END);
// lets check if it is really correct type
if (keyGetType(k1) == KEY_TYPE_COLOR) printf ("correct type");
 * @endcode
 *
 * When using type_t::KEY_TYPE_DIR, this method will not set mode
 * permissions to the key. You'll have to set it manually after
 * keySetType(), calling keySetMode() with appropriate permissions.
 * Or use the keySetDir().
 *
 * @see keyGetType()
 * @see keySetDir() to see that the directory concept is independent of types
 * @param key the key object to work with
 * @param newType contains the new type
 * @return 0 on sucess
 * @return -1 on NULL pointer and when newType >= KEY_TYPE_MAX
 * @ingroup keymeta
 *
 */
int keySetType(Key *key, type_t newType)
{
	if (!key) return -1;

	if (newType >= KEY_TYPE_MAX) return -1;

	key->type=newType;
	key->flags |= KEY_FLAG_SYNC;
	return 0;
}


/*********************************************
 *    Access times methods                   *
 *********************************************/


/**
 * Get last time the key data was read from disk.
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

	atime = keyMeta (key, "atime");
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

	mtime = keyMeta (key, "mtime");
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

	ctime = keyMeta (key, "ctime");
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



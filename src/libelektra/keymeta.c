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


/**Returns the Value of a Meta-Information.
 *
 * This is a much more efficient version of keyGetMeta().
 * But unlike with keyGetMeta you are not allowed to modify
 * the resulting string.
 *
 * @code
 * @endcode
 *
 * keyMeta() can be used to get the value of the current value,
 * see iterator keyNext(), keyCurrent()
 *
 * @return 0 if the key or metaName is 0
 * @return 0 if no such metaName is found
 * @return value of Meta-Information if Meta-Information is found
 * @see keyGetMetaSize(), keyGetMeta(), keySetMeta()
 **/
const char *keyMeta(const Key *key, const char* metaName)
{
	Key *ret;
	Key *search;

	if (!key) return 0;
	if (!metaName) return 0;
	if (!key->meta) return 0;

	search = keyNew (KEY_END);
	search->key = metaName;

	ret = ksLookup(key->meta, search, 0);

	search->key = 0;
	keyDel (search);

	if (!ret) return 0; // no such metaName
	return keyValue(ret);
}

ssize_t keyGetMetaSize(const Key *key, const char* metaName)
{}

ssize_t keyGetMeta(const Key *key, const char* metaName,
	char *returnedMetaString, size_t maxSize);

/**Set a new Meta-Information.
 *
 * Will set a new Meta-Information pair consisting of
 * metaName and newMetaString.
 *
 * Will add a new Pair for Meta-Information if metaName was
 * not added up to now.
 *
 * @return -1 on error
 * @return 0 if the Meta-Information for metaName was removed
 * @return size (>0) of newMetaString if Meta-Information was
 *         successfully added
 **/
ssize_t keySetMeta(Key *key, const char* metaName,
	const char *newMetaString)
{
	Key *toSet;
	char *metaNameDup;
	char *metaStringDup;
	ssize_t size;

	if (!key) return -1;
	size = kdbiStrLen(newMetaString);

	toSet = keyNew(KEY_END);
	if (!toSet) return -1;
	metaNameDup = kdbiStrDup(metaName);
	if (!metaNameDup)
	{
		keyDel (toSet);
		return -1;
	}
	toSet->key = metaNameDup;

	if (newMetaString && size > 1)
	{
		/*Add the meta information to the key*/
		metaStringDup = kdbiStrDup(newMetaString);
		if (!metaStringDup)
		{
			keyDel (toSet);
			return -1;
		}
		toSet->data = metaStringDup;
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
	return size;
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
 * A fresh key will have (uid_t)-1 also known as the user nobody.
 * It means that the key is not related to a user ID at the moment.
 *
 * @param key the key object to work with
 * @return the system's UID of the key
 * @return (uid_t)-1 on NULL key or currently unknown ID
 * @see keyGetGID(), keySetUID(), keyGetOwner()
 * @ingroup keymeta
 */
uid_t keyGetUID(const Key *key)
{
	if (!key) return (uid_t)-1;

	return key->uid;
}



/**
 * Set the user ID of a key.
 *
 * See @ref UID for more information about user IDs.
 *
 * @param key the key object to work with
 * @param uid the user ID to set
 * @return 0 on success
 * @return -1 on NULL key
 * @see keySetGID(), keyGetUID(), keyGetOwner()
 * @ingroup keymeta
 */
int keySetUID(Key *key, uid_t uid)
{
	if (!key) return -1;

	key->uid=uid;
	key->flags |= KEY_FLAG_SYNC;

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
	if (!key) return (gid_t)-1;

	return key->gid;
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
	if (!key) return -1;

	key->gid=gid;
	key->flags |= KEY_FLAG_SYNC;

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
	if (!key) return (time_t)-1;

	return key->atime;
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
	if (!key) return -1;

	key->atime = atime;
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
	if (!key) return (time_t)-1;

	return key->mtime;
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
	if (!key) return -1;

	key->mtime = mtime;
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
	if (!key) return (time_t)-1;

	return key->ctime;
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
	if (!key) return -1;

	key->ctime = ctime;
	return 0;
}



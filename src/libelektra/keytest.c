 /***************************************************************************
                      keytest.c  -  Methods for Key manipulation
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
 * @defgroup keytest Key :: Methods for Making Tests
 * @brief Methods to do various tests on Keys
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
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



/**
 * Ask if key is marked for stat only.
 *
 * Ask if the key will be stat instead of get it from the key database
 * completely doing kdbGetKey() or kdbGet(). This is useful
 * if you are not interested in the value, comment or key type.
 *
 * @see keyStat(), kdbGet()
 * @param key the key object to work with
 * @return 1 if it is marked, 0 otherwise
 * @return -1 on NULL pointer
 * @ingroup keytest
 **/
int keyNeedStat(const Key *key)
{
	if (!key) return -1;

	return (key->flags & KEY_FLAG_STAT) == KEY_FLAG_STAT;
}



/**
 * Test if a key needs to be synced to backend storage.
 *
 * If any key modification took place the key will be flagged with
 * KEY_FLAG_SYNC so that kdbSet() knows which keys were modified
 * and which not.
 *
 * After keyNew() the flag will normally be set, but after kdbGet()
 * and kdbSet() the flag will be removed. When you modify the key
 * the flag will be set again.
 *
 * In your application you can make use of that flag to know
 * if you changed something in a key after a kdbGet() or kdbSet().
 *
 * @note Note that also changes in the meta data will set that flag.
 *
 * @see keyNew()
 * @param key the key object to work with
 * @return 1 if @p key was changed in memory, 0 otherwise
 * @return -1 on NULL pointer
 * @ingroup keytest
 */
int keyNeedSync(const Key *key)
{
	if (!key) return -1;

	return (key->flags & KEY_FLAG_SYNC) == KEY_FLAG_SYNC;
}


/**
 * Ask if key is marked for permanent remove.
 *
 * Ask if the key will be removed instead of writing in the key database
 * when doing kdbSetKey() or kdbSet().
 *
 * @see keyRemove()
 * @see kdbSet(), kdbSetKey(), kdbRemove()
 * @param key the key object to work with
 * @return 1 if it is marked, 0 otherwise
 * @return -1 on NULL pointer
 * @ingroup keytest*/
int keyNeedRemove(const Key *key)
{
	if (!key) return -1;

	return (key->flags & KEY_FLAG_REMOVE) == KEY_FLAG_REMOVE;
}



/**
 * Check whether a key is under the @p system namespace or not
 *
 * @param key the key object to work with
 * @return 1 if key name begins with @p system, 0 otherwise
 * @return -1 on NULL pointer
 * @see keyIsUser(), keySetName(), keyName()
 * @ingroup keytest
 *
 */
int keyIsSystem(const Key *key)
{
	if (!key) return -1;

	if (key->key) return keyNameIsSystem(key->key);
	else return 0;
}




/**
 * Check whether a key is under the @p user namespace or not.
 *
 * @param key the key object to work with
 * @return 1 if key name begins with @p user, 0 otherwise
 * @return -1 on NULL pointer
 * @see keyIsSystem(), keySetName(), keyName()
 * @ingroup keytest
 *
 */
int keyIsUser(const Key *key)
{
	if (!key) return -1;

	if (key->key) return keyNameIsUser(key->key);
	else return 0;
}



/**
 * Check if the key check is below the key key or not.
 *
 * @code
Example:
key user/sw/app
check user/sw/app/key

returns true because check is below key

Example:
key user/sw/app
check user/sw/app/folder/key

returns also true because check is indirect below key
 * @endcode
 *
 * @param key the key object to work with
 * @param check the key to find the relative position of
 * @return 1 if check is below key
 * @return 0 if it is not below or if it is the same key
 * @see keySetName(), keyGetName(), keyIsDirectBelow()
 * @ingroup keytest
 *
 */
int keyIsBelow(const Key *key, const Key *check)
{
	const char * keyname = 0;
	const char * checkname = 0;
	ssize_t keysize = 0;
	ssize_t checksize = 0;

	if (!key || !check) return -1;

	keyname = keyName(key);
	checkname = keyName(check);
	keysize = keyGetNameSize(key);
	checksize = keyGetNameSize(check);

	if (keysize > checksize + 1) return 0;
	if (strncmp (keyname, checkname, keysize - 1)) return 0;
	if (checkname[keysize - 1] != '/') return 0;
	return 1;
}

/**
 * Check if the key check is direct below the key key or not.
 *
 * @code
Example:
key user/sw/app
check user/sw/app/key

returns true because check is below key

Example:
key user/sw/app
check user/sw/app/folder/key

does not return true, because there is only a indirect relation
 * @endcode
 *
 * @param key the key object to work with
 * @param check the key to find the relative position of
 * @return 1 if check is below key
 * @return 0 if it is not below or if it is the same key
 * @return -1 on null pointer
 * @see keyIsBelow(), keySetName(), keyGetName()
 * @ingroup keytest
 *
 */
int keyIsDirectBelow(const Key *key, const Key *check)
{
	const char * checkname = 0;
	ssize_t keysize = 0;

	if (!key || !check) return -1;

	checkname = keyName(check);
	keysize = keyGetNameSize(key);

	if (!keyIsBelow(key, check)) return 0;
	if (strchr(checkname + keysize, '/')) return 0;
	return 1;
}


/**
 * Check whether a key is inactive or not.
 *
 * In elektra terminology any key is inactive if the
 * it's basename starts with '.'. Inactive keys
 * must not have any meaning to applications, they
 * are reserved for users and administrators.
 *
 * To remove a whole hierarchy in elektra, don't
 * forget to pass option_t::KDB_O_INACTIVE to
 * kdbGet() to receive the inactive keys in order
 * to remove them.
 *
 * Otherwise you should not fetch these keys.
 *
 * @param key the key object to work with
 * @return 1 if the key is inactive, 0 otherwise
 * @return -1 on NULL pointer or when key has no name
 * @ingroup keytest
 *
 */
int keyIsInactive (const Key *key)
{
	char *name = 0;

	if (!key) return -1;

	name = strrchr (keyName(key), '/');

	if (!name) return -1;

	/* the slash can't be a trailing slash
	but there might be no name at all! */
	if (name[1] == '.') return 1;
	else return 0;
}




/**
 * Check if a key is directory key.
 *
 * Folder keys may also have value and comment.
 * They are discern by having a executable bit
 * set.
 *
 * If any executable bit is set it will be recognized
 * as a directory.
 *
 * @note keyIsDir may return true even though you
 *   can't access the directory.
 *
 * To know if you can access the directory, you
 * need to check, if your
 * - user ID is equal the key's
 *   user ID and the mode & 100 is true
 * - group ID is equal the key's
 *   group ID and the mode & 010 is true
 * - mode & 001 is true
 *
 * Accessing does not mean that you can get any value or
 * comments below, see @ref mode for more information.
 *
 * @param key the key object to work with
 * @return 1 if key is a directory, 0 otherwise
 * @return -1 on NULL pointer
 * @see keySetDir(), keySetMode()
 * @ingroup keytest
 */
int keyIsDir(const Key *key)
{
	if (!key) return -1;

	return ((key->mode & KEY_DEF_DIR) != 0);
}


/**
 * Check if a key is binary type.
 *
 * The function checks if the keytype is in the range between KEY_TYPE_BINARY and
 * less than excluding KEY_TYPE_STRING. Then it will be interpreted as binary.
 *
 * Make sure to use this function and don't test the binary type another way to
 * ensure compatibility and to write less error prone programs.
 *
 * @return 1 if it is binary
 * @return 0 if it is not
 * @return -1 on NULL pointer
 * @see keySetType() for more information on types
 * @see keyGetBinary(), keySetBinary()
 * @param key the key to check
 * @ingroup keytest
 */
int keyIsBinary(const Key *key)
{
	if (!key) return -1;

	return (KEY_TYPE_BINARY <= key->type && key->type < KEY_TYPE_STRING);
}


/**
 * Check if a key is string type.
 *
 * The function checks if the keytype is larger or equal KEY_TYPE_STRING.
 * Then it will be considered as string type.
 *
 * Make sure to use this function and don't test the string type another way to
 * ensure compatibility and to write less error prone programs.
 *
 * @return 1 if it is string
 * @return 0 if it is not
 * @return -1 on NULL pointer
 * @see keySetType for more information on types
 * @see keyGetString(), keySetString()
 * @param key the key to check
 * @ingroup keytest
 */
int keyIsString(const Key *key)
{
	if (!key) return -1;

	return (key->type >= KEY_TYPE_STRING);
}





/*
 * Compare 2 keys.
 *
 * The returned flags bit array has 1s (differ) or 0s (equal) for each key
 * meta info compared, that can be logically ORed using @c #keyswitch_t flags.
 * The flags you can use are @link keyswitch_t::KEY_TYPE KEY_TYPE
 * @endlink, @link keyswitch_t::KEY_NAME KEY_NAME @endlink,
 * @link keyswitch_t::KEY_VALUE KEY_VALUE @endlink,
 * @link keyswitch_t::KEY_OWNER KEY_OWNER @endlink,
 * @link keyswitch_t::KEY_COMMENT KEY_COMMENT @endlink,
 * @link keyswitch_t::KEY_UID KEY_UID @endlink,
 * @link keyswitch_t::KEY_GID KEY_GID @endlink,
 * @link keyswitch_t::KEY_MODE KEY_MODE @endlink and
 *
 * @par A very simple example would be
 * @code
Key *key1, *key;
uint32_t changes;

// omited key1 and key2 initialization and manipulation

changes=keyCompare(key1,key2);

if (changes == 0) printf("key1 and key2 are identicall\n");

if (changes & KEY_VALUE)
	printf("key1 and key2 have different values\n");
 
if (changes & KEY_UID)
	printf("key1 and key2 have different UID\n");
 
 *
 * @endcode
 *
 * 
 * @par Example of very powerfull specific Key lookup in a KeySet:
 * @code
KDB *handle = kdbOpen();
KeySet *ks=ksNew(0);
Key *base = keyNew ("user/sw/MyApp/something", KEY_END);
Key *current;
uint32_t match;
uint32_t interests;


kdbGetByName(handle, ks, "user/sw/MyApp", 0);

// we are interested only in key type and access permissions
interests=(KEY_TYPE | KEY_MODE);

ksRewind(ks);   // put cursor in the begining
while ((curren=ksNext(ks))) {
	match=keyCompare(current,base);
	
	if ((~match & interests) == interests)
		printf("Key %s has same type and permissions of base key",keyName(current));

	// continue walking in the KeySet....
}

// now we want same name and/or value
interests=(KEY_NAME | KEY_VALUE);

// we don't really need ksRewind(), since previous loop achieved end of KeySet
ksRewind(ks);
while ((current=ksNext(ks))) {
	match=keyCompare(current,base);

	if ((~match & interests) == interests) {
		printf("Key %s has same name, value, and sync status
			of base key",keyName(current));
	}
	// continue walking in the KeySet....
}

keyDel(base);
ksDel(ks);
kdbClose (handle);
 * @endcode
 * 
 * @return a bit array pointing the differences
 * @param key1 first key
 * @param key2 second key
 * @see #keyswitch_t
 * @ingroup keytest
 */
keyswitch_t keyCompare(const Key *key1, const Key *key2)
{
	keyswitch_t ret=0;
	const char *name1 = keyName(key1);
	const char *name2 = keyName(key2);
	const char *comment1 = keyComment(key1);
	const char *comment2 = keyComment(key2);
	const char *owner1 = keyOwner(key1);
	const char *owner2 = keyOwner(key2);
	const void *value1 = keyValue(key1);
	const void *value2 = keyValue(key2);
	int remove1 = keyNeedRemove(key1);
	int remove2 = keyNeedRemove(key2);
	ssize_t size1 = keyGetValueSize(key1);
	ssize_t size2 = keyGetValueSize(key2);


	if (keyGetUID(key1) != keyGetUID(key2))        ret|=KEY_UID;
	if (keyGetGID(key1) != keyGetGID(key2))        ret|=KEY_GID;
	if (keyGetType(key1)!= keyGetType(key2))      ret|=KEY_TYPE;
	if (keyGetMode(key1)!= keyGetMode(key2))  ret|=KEY_MODE;
	if (remove1 != remove2)            ret|=KEY_REMOVE;
	if (strcmp(name1, name2))          ret|=KEY_NAME;
	if (strcmp(comment1, comment2))    ret|=KEY_COMMENT;
	if (strcmp(owner1, owner2))        ret|=KEY_OWNER;
	if (size1 != size2)                ret|=KEY_VALUE;
	if (memcmp(value1, value2, size1)) ret|=KEY_VALUE;

	return ret;
}



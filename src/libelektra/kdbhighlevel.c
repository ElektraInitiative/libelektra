/***************************************************************************
         kdbhighlevel.c  -  Highlevel interface for accessing the Key Database
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/**
 * @defgroup kdbhighlevel KDB :: High Level methods
 * @brief High level methods to access the Key database.
 *
 * To use them:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * These methods are higher level. They use kdbOpen(), kdbClose(),
 * kdbGet() and kdbSet() methods to do their
 * job, and don't have to be reimplemented for a different backend.
 *
 * These functions avoid limitations through not implemented capabilities.
 * This will of course cost some effort, so read through the description
 * carefully and decide if it is appropriate for your problem.
 *
 * Binding writers don't have to implement these functions, use features
 * of the binding language instead. But you can use these functions as
 * ideas what high level methods may be useful.
 *
 * Don't use writing single keys in a loop, prefer always writing out
 * a keyset!
 *
 */


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "kdbinternal.h"



static ssize_t kdbGetHelper(KDB *handle, KeySet *returned, Key *key,
	unsigned long options)

{
	Key *dir = keyNew(keyName(key), KEY_END);
	KDBCap * cap = kdbGetCapability(handle, key);
	Key *mp;
	Key *current;
	KeySet *keys;
	ssize_t ret;

	/* Check where the mountpoint is */
	if (kdbcGetonlyFullGet(cap))
	{
		mp = kdbGetMountpoint(handle, key);
		if (mp && strlen(keyName(mp)) > 1)
		{
			keySetName (dir, keyName(mp));
		} else {
			keySetBaseName (dir, ""); /* delete basename */
		}
	} else {
		keySetBaseName (dir, ""); /* delete basename */
	}

	if (strlen(keyName(dir)) <= 2) /* copy root (user, system) */
	{
		keySetName (dir, keyName(key));
	}

#if DEBUG && VERBOSE
	printf ("using %s\n", keyName(dir));
#endif

	/* We are ready, get the keys
	 * Without recursion it does not make sense because we stepped
	 * one level higher */
	keys = ksNew(0);
	ret=kdbGet (handle, keys, dir, options & ~KDB_O_NORECURSIVE);

	/* Now filter the keys */
	ksRewind (keys);
	while ((current = ksNext(keys)) != 0)
	{
		const char * namekey = keyName(key);
		const char * namecur = keyName(current);
		if ( (strcmp (namekey, namecur) == 0) ||
		   ( (options & KDB_O_NORECURSIVE) && keyIsDirectBelow(key, current)) ||
		   (!(options & KDB_O_NORECURSIVE) && keyIsBelow(key, current)))
		{
			ksAppendKey(returned, current);
		}
	}

	ksDel (keys);
	keyDel (dir);

	return ret;
}


/**
 * Fully retrieves the passed @p key from the backend storage.
 *
 * The backend will try to get the key, identified through its
 * name.
 *
 * It uses kdbGet() for retrieving the key and copies the found data
 * to dest.
 *
 * While kdbGetKey() is perfect for a simple get of a specific key,
 * kdbGet() and kdbGetByName() gives you more control over the
 * keyset.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param dest a pointer to a Key that has a name set
 * @return 0 on success
 * @return -1 on failure
 * @return -1 on NULL pointer
 * @see kdbSetKey() to set a single key
 * @see commandGet() code in kdb command for usage example
 * @see kdbGet() and kdbGetByName() to have more control over keyset and options
 * @ingroup kdbhighlevel
 */
int kdbGetKey(KDB *handle, Key * dest)
{
	Key * source;
	KeySet *ks = 0;
	ssize_t ret;

	if (!handle || !dest) return -1;

	ks = ksNew(0);

	ret = kdbGetHelper (handle, ks, dest, KDB_O_INACTIVE);

	if (ret == -1)
	{
		ksDel (ks);
		/*errno = KDB_ERR_NOTFOUND;*/
		return -1;
	}

	source = ksLookup (ks, dest, 0);

	if (!source)
	{
		ksDel (ks);
		/*errno = KDB_ERR_NOTFOUND;*/
		return -1;
	}

	if (keyCopy(dest, source) == -1)
	{
		ksDel (ks);
		return -1;
	}

	ksDel (ks);
	return 0;
}




/**
 * Sets @p key in the backend storage.
 *
 * While kdbSetKey() is perfect for a simple get of a specific key,
 * kdbGet() and kdbGetByName() gives you more control over the
 * keyset.
 *
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param key Key to set
 * @return 0 on success
 * @return -1 on failure
 * @return -1 on NULL pointer
 * @see kdbGetKey() to get a single key
 * @see kdbSet() for more control over keyset and options
 * @see commandSet() code in kdb command for usage example
 * @ingroup kdbhighlevel
 */
int kdbSetKey(KDB * handle, const Key *key)
{
	int rc=0;
	Key *dest=0;
	Key *parent=0;
	Key *mp = 0;
	Key *dir = 0;
	KeySet *ks = 0;
	KDBCap * cap = 0;

	if (!handle || !key) return -1;

	cap = kdbGetCapability(handle, key);
	ks = ksNew(0);
	dir = keyNew(keyName(key), KEY_END);
	mp = keyDup (kdbGetMountpoint(handle, key));


	if (kdbcGetonlyFullSet(cap))
	{
		if (mp && strlen(keyName(mp)) > 1)
		{
			keySetName (dir, keyName(mp));
		} else {
			keySetBaseName (dir, ""); /* delete basename */
		}

		if (strlen(keyName(dir)) <= 2) /* copy root (user, system) */
		{
			keySetName (dir, keyName(key));
		}

#if DEBUG && VERBOSE
		printf ("kdbGetKey using %s\n", keyName(dir));
#endif

		kdbGet (handle, ks, dir, KDB_O_NORECURSIVE | KDB_O_INACTIVE);

		dest = ksLookupByName (ks, keyName(key), 0);

		if (!dest)
		{
			parent = keyDup (key);
			keySetBaseName(parent, "");
			dest = ksLookupByName (ks, keyName(parent), 0);
			keyDel (parent);
			if (dest) keySetDir (dest);
			else {
				ksDel (ks);
				keyDel (dir);
				keyDel (mp);
				/*errno = KDB_ERR_NOTFOUND;*/
				return -1; /* no parent found */
			}
			ksAppendKey(ks, keyDup (key));
		} else keyCopy(dest, key);
	} else ksAppendKey(ks, keyDup (key));

	rc = kdbSet(handle, ks, mp, 0);
	ksDel (ks);
	keyDel (dir);
	keyDel (mp);

	if (rc >= 0) return 0;
	return -1;
}





/**
 * A high-level method to get a key value, by key name.
 *
 * This method gets a backend from any backend with kdbGetKey() and
 * extracts the string and store it into returned. It only works with
 * string keys.
 *
 * This method gives you the direct relation between a keyname and
 * the value, without any kdb specific structures. Use it when
 * you just want some values out of the kdb namespace.
 *
 * You need to know the maximum string length of the object. That
 * could be the case when you e.g. save a path which is limited
 * with MAX_PATH.
 *
 * @code
KDB *handle = kdbOpen();
char buffer [MAX_PATH];

if (kdbGetString(handle, "user/key/to/get/pathname", buffer, sizeof(buffer)) == -1)
{
	// handle error cases
} else {
	printf ("The keys value is %s\n", buffer);
}
kdbClose(handle);
 * @endcode
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param keyname the name of the key to receive the value
 * @param returned a buffer to put the key value
 * @param maxSize the size of the buffer
 * @return 0 on success
 * @return -1 on failure
 * @return -1 on NULL pointers
 * @return -1 if maxSize is 0 or larger than SSIZE_MAX
 * @see kdbSetString() and kdbRemove() to set and remove a string
 * @see kdbGetKey(), keySetKey() to work with Keys
 * @see kdbGet() and kdbGetByName() for full access to internal datastructures
 * @ingroup kdbhighlevel
 *
 */
int kdbGetString(KDB * handle,const char *keyname,
		char *returned,size_t maxSize)
{
	Key *key = 0;
	int rc = 0;

	if (!handle || !keyname || !returned) return -1;
	if (!maxSize) return -1;
	if (maxSize > SSIZE_MAX) return -1;

	key=keyNew(0);
	if (!key) return -1;
	rc=keySetName(key,keyname);
	if (rc == -1)
	{
		keyDel (key);
		return -1;
	}

	rc=kdbGetKey(handle, key);
	if (rc == 0) rc = keyGetString(key,returned,maxSize);

	keyDel(key);
	return rc;
}



/**
 * A high-level method to set a value to a key, by key name.
 *
 * It will check if key exists first, and keep its metadata.
 * So you'll not loose the previous key comment.
 *
 * This will set a text key. So if the key was previously a binary
 * it will be retyped as string.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param keyname the name of the key to receive the value
 * @param value the value to be set
 * @return 0 on success
 * @return -1 on NULL pointers
 * @return -1 on failure
 * @see kdbGetString(), keySetString(), kdbSetKey()
 * @ingroup kdbhighlevel
 */
int kdbSetString(KDB * handle, const char *keyname, const char *value)
{
	Key *key = 0;
	int rc = 0;

	if (!handle || !keyname || !value) return -1;

	key=keyNew(0);
	if (!key) return -1;
	rc=keySetName(key,keyname);
	if (rc == -1)
	{
		keyDel (key);
		return -1;
	}

	kdbGetKey(handle,key);
	keySetString(key,value);
	rc=kdbSetKey(handle,key);
	keyDel(key);
	return rc;
}


/**
 * Remove a key by its name from the backend storage.
 *
 * With kdbSetString() its only possible to set a key with an empty
 * string. To really remove a key in a highlevel way you can use
 * this method.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param keyname the name of the key to be removed
 * @return 0 on success
 * @return -1 on failure
 * @return -1 on NULL pointers
 * @see together with kdbSetString() and kdbGetString() a highlevel interface for kdb
 * @see commandRemove() code in kdb command for usage example
 * @ingroup kdbhighlevel
 */
int kdbRemove(KDB *handle, const char *keyname)
{
	int rc=0;
	Key *key=0;

	if (!handle || !keyname) return -1;

	key=keyNew(0);
	if (!key) return -1;
	rc=keySetName(key,keyname);
	if (rc == -1)
	{
		keyDel(key);
		return -1;
	}

	/*TODO Implement, pseudocode:
	  kdbGet(keys);
	  ret = ksLookup(keys, key, KDB_O_POP);
	  keyDel (ret); // remove that key
	  kdbSet(keys);
	 */

	keyDel(key);

	return rc;
}



/**
 * This method is similar kdbGet() but the path is given
 * by a string.
 *
 * When it is not possible to make a key out of that string
 * -1 is returned .
 *
 * When parentName starts with / cascading will be used and both
 * keys from user and system will be fetched.
 *
 * A typically app with about 3000 keys may have this line:
 *
 *@code
KDB *handle = kdbOpen();
KeySet *myConfig = (4096, KS_END);
ssize_t ret = kdbGetByName (handle, myConfig, "/sw/app/current", 0);

// check ret and work with keyset myConfig

ksDel (myConfig);
kdbClose (handle);
 *@endcode
 *
 * myConfig will be loaded with keys from system/sw/app/current but
 * also user/sw/app/current.
 *
 * When one of these kdbGet() fails -1 will be returned, but the other
 * kdbGet() will be tried too.
 *
 * @param handle contains internal information of @link kdbOpen() opened @endlink key database
 * @param name the name where to get the keys below
 * @param returned the (pre-initialized) KeySet returned with all keys found
 * @param options ORed options to control approaches
 *   Unlike to kdbGet() is KDB_O_POP set per default.
 * @return number of keys contained by @p returned
 * @return -1 on failure
 * @return -1 when @p name is no valid key
 * @return -1 on NULL pointer
 * @see kdbGet()
 * @ingroup kdbhighlevel
 */
ssize_t kdbGetByName(KDB *handle, KeySet *returned, const char *name,
		option_t options)
{
	Key *parentKey;
	ssize_t ret, ret2;
	char *newname;

	if (!handle || !returned || !name) return -1;

	if (name[0] == '/')
	{
		newname = elektraMalloc (strlen (name) + sizeof ("system") + 1);
		if (!newname)
		{
			/*errno = KDB_ERR_NOMEM;*/
			return -1;
		}
		strncpy (newname+2, "user", 4);
		strcpy  (newname+6, name);
		parentKey = keyNew (newname+2, KEY_END);
		ret = kdbGetHelper(handle, returned, parentKey, options & ~KDB_O_DEL);
		keyDel (parentKey);

		strncpy (newname, "system",6);
		parentKey = keyNew (newname, KEY_END);
		ret2 = kdbGetHelper(handle, returned, parentKey, options & ~KDB_O_DEL);
		keyDel (parentKey);

		elektraFree (newname);
		if (ret == -1 || ret2 == -1) return -1;
		else return returned->size;
	} else {
		parentKey=keyNew(name,KEY_END);
		if (parentKey == 0)
		{
			/*errno = KDB_ERR_NOKEY;*/
			return -1;
		}
		ret = kdbGet(handle, returned,(Key *) parentKey, (options & ~KDB_O_DEL) | KDB_O_POP);
		keyDel(parentKey);
		if (ret == -1) return -1;
		else return returned->size;
	}
}

/***************************************************************************
            libkdb.c  -  Interfaces for accessing the Key Database
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




/* Subversion stuff

$Id: libkdb.c 752 2006-04-24 12:55:12Z aviram $

*/




/**
 * @defgroup kdbhelpers KeyDB :: High Level methods
 * @brief High level methods to access the Key database.
 *
 * To use them:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * These methods are higher level. They use the above methods to do their
 * job, and don't have to be reimplemented for a different backend.
 *
 * Binding writers don't have to implement these functions, use features
 * of the binding language instead. But you can use these functions as
 * ideas what high level methods may be useful.
 *
 * 
 */


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_ICONV
#include <iconv.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif


#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <pthread.h>


/* kdbbackend.h will include kdb.h and kdbprivate.h */
#include "kdbbackend.h"
#include "kdbLibLoader.h"

/* usleep doesn't exist on win32, so we use Sleep() */
#ifdef WIN32
#define usleep(x) Sleep(x)
#endif






/**
 * A high-level method to get a key value, by key name.
 * This method is valid only for string keys.
 * You have to use other methods to get non-string keys.
 *
 * @param keyname the name of the key to receive the value
 * @param returned a buffer to put the key value
 * @param maxSize the size of the buffer
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see kdbSetValue(), kdbGetKey(), kdbGetValueByParent(), keyGetString()
 * @ingroup kdbhelpers
 *
 */
int kdbGetValue(KDBHandle handle,const char *keyname,
		char *returned,size_t maxSize) {
	Key *key;
	int rc=0;

	key=keyNew(keyname,KEY_SWITCH_END);
	rc=kdbGetKey(handle, key);
	if (rc == 0) keyGetString(key,returned,maxSize);
	else rc=errno; /* store errno before a possible change */
	keyDel(key);
	errno=rc;
	return rc;
}



/**
 * A high-level method to set a value to a key, by key name.
 * It will obviously check if key exists first, and keep its metadata.
 * So you'll not loose the precious key comment.
 *
 * This will set a text key. So if the key was previously a binary, etc key, it will be retyped as text.
 *
 * @param keyname the name of the key to receive the value
 * @param value the value to be set
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * 	KDB_RET_TYPEMISMATCH if key is a directory
 * @see kdbGetValue(), keySetString(), kdbSetKey()
 * @ingroup kdbhelpers
 */
int kdbSetValue(KDBHandle handle, const char *keyname, const char *value) {
	Key *key;
	int rc;

	key=keyNew(keyname,KEY_SWITCH_END);
	rc=kdbGetKey(handle,key);
	if (! keyIsDir (key))
	{
		keySetString(key,value);
	} else {
		errno = KDB_RET_TYPEMISMATCH;
		keyDel(key);
		return -1;
	}
	rc=kdbSetKey(handle,key);
	keyDel(key);
	return rc;
}



/**
 * Fill up the @p returned buffer with the value of a key, which name
 * is the concatenation of @p parentName and @p baseName.
 *
 * @par Example:
 * @code
char *parent="user/sw/MyApp";
char *keys[]={"key1","key2","key3"};
char buffer[150];   // a big buffer
int c;

for (c=0; c<3; c++) {
	kdbGetValueByParent(handle,parent,keys[c],buffer,sizeof(buffer));
	// Do something with buffer....
}

 * @endcode
 *
 * @param parentName the name of the parent key
 * @param baseName the name of the child key
 * @param returned pre-allocated buffer to be filled with key value
 * @param maxSize size of the \p returned buffer
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see kdbGetKeyByParent()
 * @ingroup kdbhelpers
 */
int kdbGetValueByParent(KDBHandle handle, const char *parentName, const char *baseName, char *returned, size_t maxSize) {
	char *name;
	int retval=0;
	name = (char *)malloc(sizeof(char)*(strblen(parentName)+strblen(baseName)));

	sprintf(name,"%s/%s",parentName,baseName);
	retval = kdbGetValue(handle,name,returned,maxSize);
	free(name);
	return retval;
}



/**
 * Sets the provided @p value to the key whose name is the concatenation of
 * @p parentName and @p baseName.
 *
 * @param parentName the name of the parent key
 * @param baseName the name of the child key
 * @param value the value to set
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @ingroup kdbhelpers
 */
int kdbSetValueByParent(KDBHandle handle, const char *parentName, const char *baseName, const char *value) {
	char *name;
	int retval=0;
	name = (char *)malloc(sizeof(char)*(strblen(parentName)+strblen(baseName)));

	sprintf(name,"%s/%s",parentName,baseName);
	retval = kdbSetValue(handle,name,value);
	free(name);
	return retval;
}



/**
 * Given a parent key name plus a basename, returns the key.
 *
 * So here you'll provide something like
 * - @p system/sw/myApp plus @p key1 to get @p system/sw/myApp/key1
 * - @p user/sw/MyApp plus @p dir1/key2 to get @p user/sw/MyApp/dir1/key2
 *
 * @param parentName parent key name
 * @param baseName leaf or child name
 * @param returned a pointer to an initialized key to be filled
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see kdbGetKey(), kdbGetValueByParent(), kdbGetKeyByParentKey()
 * @ingroup kdbhelpers
 */
int kdbGetKeyByParent(KDBHandle handle, const char *parentName, const char *baseName, Key *returned) {
	char *name;
	name = (char *)malloc(sizeof(char) * (strblen(parentName)+strblen(baseName)));

	sprintf(name,"%s/%s",parentName,baseName);	
	keySetName(returned,name);
	free(name);
	return kdbGetKey(handle,returned);
}


/**
 * Similar to previous, provided for convenience.
 * @param parent pointer to the parent key
 * @see kdbGetKey(), kdbGetKeyByParent(), kdbGetValueByParent()
 * @return 0 on success, or what kdbGetKey() returns, and @c errno is set
 * @ingroup kdbhelpers
 */
int kdbGetKeyByParentKey(KDBHandle handle, const Key *parent, const char *baseName, Key *returned) {
	size_t size=keyGetFullNameSize(parent);
	char *name;
	name = (char *)malloc(sizeof(char) * (size+strblen(baseName)));

	keyGetFullName(parent,name,size);
	name[size-1]='/';
	strcpy((char *)(name+size),baseName);

	keySetName(returned,name);
	free(name);
	return kdbGetKey(handle,returned);
}


/**
 * This method is similar and calls kdbGetKeyChildKeys().
 * It is provided for convenience.
 * 
 * Instead of passing the parentName with a key it directly
 * uses a string.
 * 
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated from kdbGetKeyChildKeys()
 * @see kdbGetKeyChildKeys()
 * @ingroup kdbhelpers
 */
ssize_t kdbGetChildKeys(KDBHandle handle, const char *parentName, KeySet *returned, unsigned long options) {
	/*TODO: make this a non helper, but kdbGetKeyChildKeys a helper*/
	Key *parentKey;
	ssize_t rc;
	
	parentKey=keyNew(parentName,KEY_SWITCH_END);
	rc=kdbGetKeyChildKeys(handle,parentKey,returned,options);
	
	keyDel(parentKey);
	
	return rc;
}



/**
 * Returns a KeySet with all root keys currently recognized and present
 * on the system. Currently, the @p system and current user's @p user keys
 * are returned.
 *
 * @param returned the initialized KeySet to be filled
 * @return the number of root keys found
 * @return -1 on failure and @c errno is propagated
 * @see #KeyNamespace
 * @see commandList() code in kdb command for usage example
 * @ingroup kdbhelpers
 *
 */
ssize_t kdbGetRootKeys(KDBHandle handle, KeySet *returned) {
	Key *system=0,*user=0;

	user=keyNew("user",KEY_SWITCH_NEEDSYNC,handle,
		KEY_SWITCH_END);
	if (user->flags & KEY_SWITCH_FLAG) {
		keyDel(user);
		user=0;
	} else ksInsert(returned,user);

	system=keyNew("system",KEY_SWITCH_NEEDSYNC,handle,
		KEY_SWITCH_END);
	if (system->flags & KEY_SWITCH_FLAG) {
		keyDel(system);
		system=0;
	} else ksInsert(returned,system);

	return returned->size;
}


/**
 * Remove a key by its name from the backend storage.
 * 
 * This is a convenience to kdbRemoveKey().
 *
 * @param keyName the name of the key to be removed
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see commandRemove() code in kdb command for usage example
 * @ingroup kdbhelpers
 */
int kdbRemove(KDBHandle handle, const char *keyName) {
	/*TODO: make this a non helper, but kdbGetKeyChildKeys a helper*/
	int rc=0;
	Key *key=0;
	
	key=keyNew(KEY_SWITCH_END);
	rc=keySetName(key,keyName);
	if (rc == 0) {
		keyDel(key);
		return -1; /* propagate errno */
	}
	
	rc=kdbRemoveKey(handle,key);
	keyDel(key);
	
	return rc;
}

/**
 * @mainpage The Elektra API
 *
 * @section overview Elektra Initiative Overview
 *
 * Elektra is an initiative to unify Linux/Unix configurations. It does that
 * providing an hierarchical namespace to store configuration keys and
 * their values, an API to access/modify them, and command line tools.
 *
 * Everything about the initiative can be found at http://www.libelektra.org
 *
 * @section using Using the Elektra Library
 *
 * A C or C++ source file that wants to use Elektra should include:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * There is also a library that provides some
 * @ref tools "optional XML manipulation methods called KDB Tools", and to use
 * it you should include:
 * @code
 * #include <kdbtools.h>
 * @endcode
 *
 * To link an executable with the Elektra library, the correct way is to
 * use the @c pkg-config tool:
 * @code
 * bash$ cc `pkg-config --libs elektra` -o myapp myapp.c
 * @endcode
 *
 * Or, if you don't have @c pkg-config:
 * @code
 * bash$ cc -L /lib -lelektra -o myapp myapp.c
 * @endcode
 *
 * @section classes Elektra API
 *
 * The API was written in pure C because Elektra was designed to be useful
 * even for the most basic system programs, which are all made in C. Also,
 * being C, bindings to other languages can appear, as we already have for
 * Python, Ruby, etc.
 *
 * The API follows an Object Oriented design, and there are only 3 classes
 * as shown by the figure:
 *
 * @image html classes.png "Elektra Classes"
 *
 * Some general things you can do with each class are:
 *
 * @subsection KeyDB KeyDB
 *   - @link kdbGetKey() Retrieve @endlink and @link kdbSetKey() commit
 *     @endlink Keys and @link kdbSetKeys() KeySets @endlink,
 *     @link kdbGetKeyChildKeys() recursively @endlink or not
 *   - Retrieve and commit individual @link kdbGetValue() Key value @endlink, by
 *     absolute name or @link kdbGetValueByParent() relative to parent @endlink
 *   - Monitor and notify changes in @link kdbMonitorKey() Keys @endlink and
 *     @link kdbMonitorKeys() KeySets @endlink
 *   - Create and delete regular, folder or symbolic link Keys
 *   - See @ref kdb "class documentation" for more
 *
 * @subsection Key Key
 *   - Get and Set key properties like @link keySetName() name @endlink,
 *     root and @link keySetBaseName() base name @endlink,
 *     @link keySetString() value @endlink, @link keySetType() type @endlink,
 *     @link keyGetAccess() permissions @endlink,
 *     @link keyGetMTime() changed time @endlink,
 *     @link keyGetComment() comment @endlink, etc
 *   - @link keyCompare() Make powerfull comparations of all key properties
 *     with other keys @endlink
 *   - @link keyNeedsSync() Test if changed @endlink, if it is a
 *     @link keyIsUser() @p user/ @endlink or @link keyIsSystem() @p system/
 *     @endlink key, etc
 *   - @link keySetFlag() Flag it @endlink and @link keyGetFlag() test if key
 *     has a flag @endlink
 *   - @link keyToStream() Export Keys to an XML representation @endlink
 *   - See @ref key "class documentation" for more
 *
 * @subsection KeySet KeySet
 *   - Linked list of Key objects
 *   - @link ksInsert() Insert @endlink and @link ksAppend() append @endlink
 *     entire @link ksInsertKeys() KeySets @endlink or Keys
 *   - @link ksNext() Work with @endlink its @link ksCurrent() internal
 *     cursor @endlink
 *   - @link ksCompare() Compare entire KeySets @endlink
 *   - @link ksFromXMLfile() Import @endlink and
 *     @link ksToStream() Export KeySets @endlink to an XML representation
 *   - See @ref keyset "class documentation" for more
 *
 *
 * @section keynames Key Names and Namespaces
 *
 * There are 2 trees of keys: @c system and @c user
 *
 * @subsection systemtree The "system" Subtree
 *
 * It is provided to store system-wide configuration keys, that is,
 * configurations that daemons and system services will use.
 *
 * @subsection usertree The "user" Subtree
 *
 * Used to store user-specific configurations, like the personal settings
 * of a user to certains programs
 *
 *
 * @section rules Rules for Key Names
 *
 * When using Elektra to store your application's configuration and state,
 * please keep in mind the following rules:
 * - You are not allowed to create keys right under @p system or @p user.
 * - You are not allowed to create folder keys right under @p system or @p user.
 *   They are reserved for very essential OS subsystems.
 * - The keys for your application, called say @e MyApp, should be created under
 *   @p system/sw/MyApp and/or @p user/sw/MyApp.
 *
 *
 */


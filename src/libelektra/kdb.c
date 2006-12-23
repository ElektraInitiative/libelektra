/***************************************************************************
            kdb.c  -  High level functions for accessing the Key Database
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

$Id$

*/

/**
 * @defgroup kdb KeyDB :: Class Methods
 * @brief General methods to access the Key database.
 *
 * To use them:
 * @code
 * #include <kdb.h>
 * @endcode
 *
 * The kdb*() class of methods are used to access the storage, to get and set
 * @link key Keys @endlink or @link keyset KeySets @endlink.
 *
 * They use some backend implementation to know the details about how to access
 * the storage. Currently we have this backends:
 * - @c berkeleydb: the keys are stored in a Berkeley DB database, providing
 *   very small footprint, speed, and other advantages.
 * - @c filesys: the key hierarchy and data are saved as plain text files in 
 *   the filesystem.
 * - @c ini: the key hierarchy are saved into configuration files.
 *   @see http://www.libelektra.org/Ini
 * - @c fstab: a reference backend used to interpret the @c /etc/fstab file as
 *   a set of keys under @c system/filesystems .
 * - @c gconf: makes Elektra use the GConf daemon to access keys. Only the
 *   @c user/ tree is available since GConf is not system wide.
 *
 * Backends are physically a library named @c /lib/libelektra-{NAME}.so .
 *
 * In general usage, the @c default backend will be used, which is a pointer to
 * some other backend. Your program can use a different backend simply by
 * setting the @e KDB_BACKEND environment variable. Or, if you know what you
 * are doing, you can hardcode it in your code and use the explicit
 * kdbOpenBackend() method to use one. These options should really not used,
 * thus you destroy the global namespace with that.
 *
 * When @link backend writing a new backend @endlink, these are the methods
 * you'll have to reimplement:
 * kdbOpen(), kdbClose(), kdbGetKey(), kdbSetKey(), kdbStatKey(),
 * kdbGetKeyChildKeys(), kdbRemove(), kdbRename().
 *
 * And methods that are suggested to reimplement (but not needed) if you want
 * them to get the benefits of your new backend: kdbSetKeys(),
 * kdbMonitorKey(), kdbMonitorKeys().
 *
 * Language binding writers should follow the same rules:
 * - You must relay completely on the backend-dependent methods.
 * - You may use or reimplement the second set of methods.
 * - You should completely reimplement in your language the higher
 *   lever methods.
 * - Many methods are just for comfort in C. These methods are marked
 *   and need not to be implemented if the binding language has e.g. string
 *   operators which can do the operation easily.
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


/* kdbbackend.h will include kdb.h and kdbprivate.h */
#include "kdbbackend.h"
#include "kdbprivate.h"
#include "kdbLibLoader.h"

/* usleep doesn't exist on win32, so we use Sleep() */
#ifdef WIN32
#define usleep(x) Sleep(x)
#endif



struct _KDBBackend {
	/* environment for which this backend was opened */
	pid_t pid;
	pthread_t tid;
	uid_t uid;
	gid_t gid;
	mode_t umask;
	char *userName;
	
	/* backend specific data to carry along on kdb*() calls */
	void *backendData;
	
	/* backend name */
	char *name;
	
	
	/* These are the interfaces that must be implemented */
	
	kdbOpenPtr kdbOpen;
	kdbClosePtr kdbClose;
	
	kdbGetKeyPtr kdbGetKey;
	kdbSetKeyPtr kdbSetKey;
	kdbStatKeyPtr kdbStatKey;
	kdbRenamePtr kdbRename;
	kdbRemoveKeyPtr kdbRemoveKey;
	kdbGetChildKeysPtr kdbGetKeyChildKeys;
	
	
	/* These are the optional methods */
	
	kdbSetKeysPtr kdbSetKeys;
	kdbMonitorKeyPtr kdbMonitorKey;
	kdbMonitorKeysPtr kdbMonitorKeys;
	
	/* dynamic libloader data */
	kdbLibHandle dlHandle;
};




/**
 * Opens the session with the Key database, using a backend defined by
 * environment var @e $KDB_BACKEND. If the environment is not set
 * the @e default backend will be opened.
 *
 * You must always call this method before retrieving or commiting any
 * keys to the database. In the end of the program,
 * after using the key database, you must not forget to kdbClose().
 * You can use the atexit () handler for it.
 *
 * This is the best way to have affairs with the key database, unless
 * the program is concerned about security and authentication (e.g. su,
 * login, telnetd, etc), in which kdbOpenDefault() should be used. kdbOpen()
 * is used by the @c kdb command.
 *
 * The @p handle parameter will be initialized with an environment, and it
 * should be passed along on any kdb*() method your application calls.
 *
 * You don't need to use any of the kdbOpen*() methods if you only want to
 * manipulate plain in-memory Key or KeySet objects without any affairs with
 * the backend key database,
 *
 * @param handle the key database handler to initialize
 * @see kdbOpenBackend(), kdbOpenDefault(), kdbClose()
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated from kdbOpenBackend()
 *  using kdbOpen.
 * @ingroup kdb
 */
int kdbOpen(KDBHandle *handle) {
	char *backendName=0;
	
	backendName=getenv("KDB_BACKEND");
	if (backendName) return kdbOpenBackend(handle,backendName);
	else return kdbOpenBackend(handle,DEFAULT_BACKEND);
}



/**
 * Opens the session with the Key database.
 *
 * Different from kdbOpen(), kdbOpenDefault() will completely ignore
 * the @e $KDB_BACKEND environment and open the @e default backend.
 * So kdbOpenDefault() must be used by programs concerned about security
 * (e.g. su, login, sshd, etc).
 *
 * The @e default backend use to be a symlink to the real backend, and
 * is found in /lib/libelektra-default.so
 *
 * The @p handle parameter will be initialized with an environment, and it
 * should be passed along on any kdb*() method your application calls.
 *
 * @param handle the key database handler to initialize
 * @see kdbOpen(), kdbOpenBackend(), kdbClose()
 * @return 0 on success
 * @return -1 on failure and @errno is set
 *  using kdbOpen.
 * @ingroup kdb
 */
int kdbOpenDefault(KDBHandle *handle) {
	return kdbOpenBackend(handle,DEFAULT_BACKEND);
}




/**
 * Opens the session with the Key database, dynamically loading a specific
 * beckend for libelektra.so.
 * 
 * After dynamic loading, the backend will be initialized with its
 * implementation of kdbOpen().
 * 
 * The @p handle parameter will be initialized with an environment, and it
 * should be passed along on any kdb*() method your application calls.
 *
 * @param handle the key database handler to initialize
 * @param backendName used to define the module filename as
 * 	libelektra-@p "backendName".so
 * @return 0 on success.
 * @return -1 on failure and @c errno is set to 
 * 	- KDBErr::KDB_RET_EBACKEND if backend library could not be opened
 * 	- KDBErr::KDB_RET_NOSYS if backend library doesn't provide the
 * 	  essential "kdbBackendFactory" initialization symbol
 * 	- KDBErr::KDB_RET_NOSYS if backend failed to export its methods
 * 	- KDBErr::KDB_RET_NOSYS if backend does not provide a kdbOpen()
 * 	  implementation
 * @see kdbOpen()
 * @par Example of copying keys from one backend to another
 * @code
KeySet *ks=ksNew();
KDBHandle handle;

kdbOpen(&handle); // open default backend
kdbGetChildKeys(handle,"system/sw/httpd",ks, 
	KDB_O_NFOLLOWLINK |  // we want real links, not their targets
	KDB_O_INACTIVE |     // even commented (inactive) keys
	KDB_O_DIR |          // even pure directory keys
	KDB_O_RECURSIVE |    // all of this recursivelly
	KDB_O_SORT);         // sort all
kdbClose(&handle);

kdbOpenBackend(&handle,"apache");

// The hipotethical libelektra-apache.so backend implementation for kdbSetKeys()
// simply interprets the passed KeySet and generates an old style
// equivalent /etc/httpd/httpd.conf file.
kdbSetKeys(handle,ks);
kdbClose(&handle);

ksDel(ks);
 * @endcode
 * @par Emulating same bahavior of previous example but now with the kdb command
 * @code
bash# kdb export system/sw/httpd > apacheconf.xml
bash# KDB_BACKEND=apache kdb import apacheconf.xml
 * @endcode
 * @ingroup kdb
 */
int kdbOpenBackend(KDBHandle *handle, char *backendName) {
	/* TODO: review error codes on errno */
	kdbLibHandle dlhandle=0;
	char backendlib[300];
	KDBBackendFactory kdbBackendFactory=0;
	int rc=0;
	
	*handle=0;
	
	/* load the environment and make us aware of codeset conversions */
	#ifdef HAVE_SETLOCALE
	setlocale(LC_ALL,"");
	#endif
	
	/* init */
	if ( (rc = kdbLibInit()) ) {
		errno=KDB_RET_NOSYS;
		return -1; /* error */
	}	
	
	sprintf(backendlib,"libelektra-%s",backendName);
	dlhandle=kdbLibLoad(backendlib);
	if (dlhandle == 0) {
		errno=KDB_RET_EBACKEND;
		return -1; /* error */
	}
	
	/* load the "kdbBackendFactory" symbol from backend */
	/*kdbBackendFactory=(KDBBackendFactory)kdbLibSym(dlhandle,
	*(void **)(&kdbBackendFactory)=kdbLibSym(dlhandle,*/
	kdbBackendFactory=kdbLibSym(dlhandle,
		"kdbBackendFactory");
	if (kdbBackendFactory == 0) {
		errno=KDB_RET_NOSYS;
		return -1; /* error */
	}
	
	*handle=(*kdbBackendFactory)();
	if ((*handle) == 0) {
		fprintf(stderr,"libelektra: Can't initialize \"%s\" backend\n",
			backendName);
		errno=KDB_RET_NOSYS;
		return -1; /* error */
	}

	/* save the libloader handle for future use */
	(*handle)->dlHandle=dlhandle;
	
	/* Give some context to the handle in the case backend is remote */
	(*handle)->pid      = getpid();
	(*handle)->uid      = getuid();
	(*handle)->gid      = getgid();
	kdbhSetUserName(*handle, getenv("USER"));
	(*handle)->umask    = umask(0); umask((*handle)->umask);
	
	/* let the backend initialize itself */
	if ((*handle)->kdbOpen) rc=(*handle)->kdbOpen(handle);
	else {
		errno=KDB_RET_NOSYS;
		rc=-1;
	}
	return rc;
}



/**
 * Closes the session with the Key database.
 *
 * You should call this method when you finished your affairs with the key
 * database. You can manipulate Key and KeySet objects also after kdbClose().
 * You must not use any kdb* call afterwards. You can implement kdbClose()
 * in the atexit() handler.
 *
 * This is the counterpart of kdbOpen().
 *
 * The @p handle parameter will be finalized and all resources associated to it
 * will be freed. After a kdbClose(), this @p handle can't be used anymore,
 * unless it gets initialized again with another call to kdbOpen().
 *
 * @param handle the key database handler to initialize
 * @see kdbOpen()
 * @return 0 on success
 * @return -1 on failure, and @c errno is set.
 * 	If the backend implementation of kdbOpen can't be found, @c errno is
 * 	set to KDBErr::KDB_RET_NOSYS.
 * @ingroup kdb
 */
int kdbClose(KDBHandle *handle) {
	int rc=0;
	
	if (*handle && (*handle)->kdbClose) rc=(*handle)->kdbClose(handle);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	if (rc == 0) {
		kdbLibClose((*handle)->dlHandle);
		free((*handle)->name);
		free((*handle)->userName);
		free(*handle); *handle=0;
	}
	
	return rc;
}






/**
 * Retrieve a number of inter-related keys at once.
 * This is one of the most practical methods of the library, and you should
 * use it to retrieve in one shot all keys needed by your application.
 *
 * The @p returned KeySet must be initialized or may already contain some
 * keys (in this case, the new retrieved keys will be simply appended).
 * 
 * In default behaviour (@p options = 0) it will fully retrieve all keys
 * under the @p parentKey folder, except folders (and their children) and
 * inactive keys.
 * 
 * The parent key per se will not be retrieved. This is the job of the caller.
 * 
 * The @p option is an array of the following ORed flags:
 * - @p KDBOptions::KDB_O_RECURSIVE \n
 *   Retrieve also the keys under the folder keys, under the specified
 *   @p parentKey, recursively.
 * - @p KDBOptions::KDB_O_DIR \n
 *   Also include folders in the @p returned KeySet, otherwise only value
 *   keys will be retrieved.
 * - @p KDBOptions::KDB_O_DIRONLY \n
 *   Put in @p returned only the folder keys. The resulting 
 *   KeySet will be only the skeleton of the tree. This option must be
 *   ORed together with KDB_O_DIR.
 * - @p KDBOptions::KDB_O_STATONLY \n
 *   Only stat the keys. It means that key value, comment and data type will
 *   not be retrieved. The resulting keys will contain only meta info such
 *   as user and group IDs, owner, access permissions and modification times.
 * - @p KDBOptions::KDB_O_INACTIVE \n
 *   Will make it not ignore inactive keys, so @p returned will contain also
 *   inactive keys. Inactive keys are those that have names
 *   begining with '.' (dot).
 * - @p KDBOptions::KDB_O_SORT \n
 *   Causes @p returned to be ksSort()ed.
 *
 * @par Example:
 * @code
char errormsg[300];
KDBHandle handle;
KeySet *myConfig;
Key *key;

key=keyNew("system/sw/MyApp",KEY_SWITCH_END);
myConfig=ksNew();

kdbOpen(&handle);
rc=kdbGetKeyChildKeys(handle,key, myConfig, KDB_O_RECURSIVE);
keyDel(key); // free this resource.... we'll use it later
kdbClose(&handle);

// Check and handle propagated error
if (rc) switch (errno) {
	case KDB_RET_INVALIDKEY:
		sprintf(errormsg,"Something invalid");
		kdbPrintError(errormsg);
		break;
	case KDB_RET_NOTFOUND:
		fprintf(stderr,"Key not found"); // custom error message
		break;
	default:
		sprintf(errormsg,"My application");
		kdbPrintError(errormsg);
		break;
}

ksRewind(myConfig); // go to begining of KeySet
key=ksNext(myConfig);

while (key) {
	// do something with each key . . .

	key=ksNext(myConfig); // next key
}
 * @endcode
 *
 * @param parentKey parent key
 * @param returned the (pre-initialized) KeySet returned with all keys found
 * @param options ORed options to control approaches
 * @see #KDBOptions, #KDBErr
 * @see kdbGetChildKeys() for a convenience method
 * @see ksLookupByName(), ksLookupRE(), ksLookupByValue() for powerfull
 * 	lookups after the KeySet was retrieved
 * @see ksSort() for what is done when you ask for KDBOptions::KDB_O_SORT
 * @see commandList() code in kdb command for usage example
 * @see commandEdit() code in kdb command for usage example
 * @see commandExport() code in kdb command for usage example
 * @return number of keys contained by @p returned or -1 on failure, @c errno
 *    is propagated and can be KDBErr::KDB_RET_INVALIDKEY,
 *    KDBErr::KDB_RET_NOTFOUND, KDBErr::KDB_RET_NOSYS
 * @ingroup kdb
 *
 */
ssize_t kdbGetKeyChildKeys(KDBHandle handle, const Key *parentKey,
		KeySet *returned, unsigned long options) {
	
	if (handle && handle->kdbGetKeyChildKeys)
		return handle->kdbGetKeyChildKeys(handle,parentKey,returned,options);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
}



/**
 * Stats the key only for its meta-info from the backend storage.
 * The key may not hold value and comment after using kdbStatKey().
 * 
 * A key of type KeyType::KEY_TYPE_LINK will have its target address loaded in the
 * @p key structure, which can be accessed later using keyStealValue() or
 * keyGetString(). This is the only way to know the target of a link key
 * without dereferencing it (in contrast to kdbGetKey(), where the link is
 * followed).
 *
 * Info like comments and key data type may not be retrieved if backend
 * supports a way not to get them.
 *
 * @param key an initialized Key pointer to be filled.
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @ingroup kdb
 */
int kdbStatKey(KDBHandle handle, Key *key) {
	int rc=0;
	
	if (handle && handle->kdbStatKey)
		rc=handle->kdbStatKey(handle,key);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}



/**
 * Fully retrieves the passed @p key from the backend storage.
 *
 * The backend will try to get the key, identified through its
 * name.
 * 
 * @param key a pointer to a Key that has a name set
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see commandGet() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbGetKey(KDBHandle handle, Key *key) {
	int rc=0;
	
	if (handle && handle->kdbGetKey)
		rc=handle->kdbGetKey(handle,key);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}



/**
 * Commits the @p ks KeySet to the backend storage, starting from @p ks's
 * current position until its end. This is why it is suggested that you call
 * ksRewind() on @p ks before calling this method.
 * 
 * Each key is checked with keyNeedsSync() before being actually commited. So
 * only changed keys are updated.
 *
 * If some error occurs, kdbSetKeys() will stop. In this situation the KeySet
 * internal cursor is left on the key that generated the error.
 *
 * @par Example of how this method must be used:
 * @code
KeySet *ks;  // the KeySet I want to set

// ommited... fill ks with some keys

ksRewind(ks);
while ((ret=kdbSetKeys(handle,ks))) {
	// We got an error. Warn user.
	Key *problem;
	char error[500]="";
	char keyname[300]="";

	problem=ksCurrent(ks);
	if (problem) keyGetFullName(problem,keyname,sizeof(keyname));
	sprintf(error,"kdb import: while importing %s", keyname);
	kdbPrintError(error);

	// And try to set keys again starting from the next key,
	// unless we reached the end of KeySet
	if (ksNext(ks) == 0) break;
}
 * @endcode
 * @param ks a KeySet full of changed keys
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see kdbSetKey(), keyNeedsSync(), ksNext(), ksCurrent()
 * @see commandEdit(), commandImport() code in kdb command for usage and error
 *       handling example
 * @ingroup kdb
 */
int kdbSetKeys(KDBHandle handle, KeySet *ks) {
	int rc=0;
	
	if (handle) {
		if(handle->kdbSetKeys)
			rc=handle->kdbSetKeys(handle,ks);
		else
			/* If backend doesn't provide kdbSetKeys, use the default */
			rc=kdbSetKeys_default(handle,ks);
	}
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}




/**
 * Sets @p key in the backend storage.
 * Directory keys will be created recursivelly if needed.
 * In case a path component is found in the storage as a regular non-dir key,
 * it will be converted into a dir key if possible.
 *
 * @see kdbGetKey(), kdbSetKeys()
 * @see commandSet() code in kdb command for usage example
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @ingroup kdb
 */
int kdbSetKey(KDBHandle handle, Key *key) {
	int rc=0;
	
	if (handle && handle->kdbSetKey)
		rc=handle->kdbSetKey(handle,key);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}




/**
 * Rename a key in the backend storage.
 *
 * key must be a fully retrieved key. If you want another
 * name its not enough to kdbSetKey() it again (old key
 * would stay, you could remove it with kdbRemoveKey though).
 * 
 * kdbRename() can do it for you, maybe with a more efficient
 * method then described above.
 *
 * @param key the key to be renamed
 * @param newName the new key name
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @ingroup kdb
 */
int kdbRename(KDBHandle handle, Key *key, const char *newName) {
	int rc=0;
	
	if (handle) {
		if ( handle->kdbRename )
			rc=handle->kdbRename(handle,key,newName);
		else
			rc=kdbRename_default(handle, key, newName);
			
	} else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}



/**
 * Remove a key from the backend storage.
 * 
 * The @c key object will not be freed. It is your responsability
 * to keyDel() it after kdbRemoveKey().
 *
 * This method is not recursive.
 *
 * @param key the key to be removed
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see commandRemove(), and ksCompare() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbRemoveKey(KDBHandle handle, const Key *key) {
	int rc=0;
	
	if (handle && handle->kdbRemoveKey)
		rc=handle->kdbRemoveKey(handle,key);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}




/**
 * Create a link key on the backend storage that points to other key.
 *
 * @param oldPath destination key name
 * @param newKeyName name of the key that will be created and will point
 * to @param oldPath
 * @return whathever is returned by kdbSetKey(), and @c errno is set
 * @see commandLink() code in kdb command for usage example
 * @see commandSet() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbLink(KDBHandle handle, const char *oldPath, const char *newKeyName) {
	Key *key;
	int rc;

	key=keyNew(newKeyName,KEY_SWITCH_END);
	keySetLink(key,oldPath);

	rc=kdbSetKey(handle,key);
	keyDel(key);

	return rc;
}




/**
 * Monitor a KeySet for some key change.
 *
 * This method will scan the @p interests KeySet, starting and finishing in
 * the KeySet's next cursor position, in a circular behavior, looking for some
 * change defined in the @p diffMask mask. It will use kdbMonitorKey()
 * and will return at the first key change ocurrence, or when requested
 * @p iterations finish.
 *
 * You may check the return code to see if some key changed, and get
 * the updated key using ksCurrent().
 *
 * @par Example:
 * @code
KeySet *myConfigs;

// key db initialization omitted

myConfigs=ksNew();
kdbGetChildKeys(handle,"system/sw/MyApp",myConfigs,KDB_O_RECURSIVE | KDB_O_SORT);

// use the keys . . . .

// now monitor any key change
ksRewind(myConfigs);
while (1) {
	Key *changed=0;
	char keyName[300];
	char keyData[300];
	uint32_t diff;

	// block until any change in key value or comment . . .
	diff=kdbMonitorKeys(handle,myConfigs,
		KEY_SWITCH_VALUE | KEY_SWITCH_COMMENT,
		0,0); // ad-infinitum

	changed=ksCurrent(myConfigs);
	keyGetName(changed,keyName,sizeof(keyName));

	switch (diff) {
		case KEY_SWITCH_FLAG:
			printf("Key %s was deleted\n",keyName);
			break;
		case KEY_SWITCH_NEEDSYNC:
			printf("No cretentials to access Key %s\n",keyName);
			break;
		default:
			keyGetString(changed,keyData,sizeof(keyData));
			printf("Key %s has changed its value to %s\n",keyName,keyData);
	}
}

ksDel(myConfigs);
 * @endcode
 *
 * @see kdbMonitorKey(), ksCurrent(), ksRewind(), ksNext(), #KeySwitch
 * @see commandMonitor() code in kdb command for usage example
 * @return diff mask on success
 * @return -1 on failure and @c errno is propagated
 * @ingroup kdb
 *
 */
uint32_t kdbMonitorKeys(KDBHandle handle, KeySet *interests, uint32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	
	uint32_t rc=0;
	
	if (handle) {
		if(handle->kdbMonitorKeys) 
			rc=handle->kdbMonitorKeys(handle,interests,diffMask,iterations,
				sleep);
		else 
			/* If backend doesn't provide kdbMonitorKeys, then use the default*/
			rc = kdbMonitorKeys_default(handle,interests,diffMask,iterations,
				sleep);
	}
	else {
		errno=KDB_RET_NOSYS;
		return 0;
	}
	
	return rc;
}


/**
 * A high level, probably inefficient, implementation for the kdbMonitorKeys()
 * method. If a backend doesn't want to reimplement this method, this
 * implementation can be used.
 *
 * @return diff mask on success 
 * @return -1 on failure and @c errno is propagated
 * @ingroup backend
 */
uint32_t kdbMonitorKeys_default(KDBHandle handle, KeySet *interests,
		uint32_t diffMask, unsigned long iterations, unsigned sleeptime) {
	Key *start,*current;
	uint32_t diff;
	int infinitum=0;

	if (!interests || !interests->size) return 0;

	/* Unacceptable 0 usecs sleep. Defaults to 1 second */
	if (!sleeptime) sleeptime=1000;

	if (!iterations) infinitum=1;
	else infinitum=0;

	current=start=ksCurrent(interests);

	while (infinitum || --iterations) {
		do {
			diff=kdbMonitorKey(handle,current,diffMask,1,0);
			if (diff) return diff;
			current=ksNext(interests);
		} while (current!=start);

		/* Test if some iterations left . . . */
		if (infinitum || iterations) usleep(sleeptime);
	}
	return 0;
}



/**
 * Monitor a key change.
 *
 * This method will block execution until one of the folowing happens:
 * - All requested @p iterations, with requested @p sleep times, finish.
 *   If no change happens, zero is returned.
 * - Requested key info and meta-info (defined by @p diffMask) changes when
 *   keyCompare()ed with the original @p interest.
 *
 * @p interest should be a full key with name, value, comments, permissions,
 * etc, and all will be compared and then masked by @p diffMask.
 *
 * If @p interest is a folder key, use @p KEY_SWITCH_TIME in @p diffMask
 * to detect a time change, so you'll know something happened (key
 * modification, creation, deletion) inside the folder.
 *
 * If @p interest was not found, or deleted, the method will return
 * immediatly a @p KEY_SWITCH_FLAG value.
 *
 * If you don't have access rights to @p interest, the method will return
 * immediatly a @p KEY_SWITCH_NEEDSYNC value.
 *
 * If something from @p diffMask has changed in @p interest, it will be
 * updated, so when method returns, you'll have an updated version of the key.
 *
 * @param interest key that will be monitored
 * @param diffMask what particular info change we are interested
 * @param iterations how many times to test. 0 means infinitum or until
 * 	some change happens
 * @param sleep time to sleep, in microseconds, between iterations.
 * 	0 defaults to 1 second.
 * @return the ORed @p KEY_SWITCH_* flags of what changed
 * @return 0 on success
 * @return -1 on failure and @c errno is propagated
 * @see #KeySwitch
 * @see keyCompare()
 * @see kdbMonitorKeys() to monitor KeySets, and for a code example
 * @see commandMonitor() code in kdb command for usage example
 * @ingroup kdb
 *
 */
uint32_t kdbMonitorKey(KDBHandle handle, Key *interest, uint32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	
	int rc=0;
	
	if (handle) {
		if(handle->kdbMonitorKey)
		  rc=handle->kdbMonitorKey(handle,interest,diffMask,iterations,sleep);
		else
			rc=kdbMonitorKey_default(handle,interest,diffMask,iterations,sleep);
	}
	else {
		errno=KDB_RET_NOSYS;
		return 0;
	}
	
	return rc;
}




/**
 * Returns a structure of information about the internals
 * of the library and the backend used.
 *
 * Currently, the returned object has the following members:
 *
 * - @p version: the version for the Elektra library
 * - @p backendName: the name of the storage backend that is or will be used
 * - @p backendIsOpen: whether the backend was already opened with kdbOpen()
 *
 * After use, the returned object must be freed with a call to kdbFreeInfo().
 *
 * @par Example:
 * @code
KDBInfo *info=0;

// key dababase initialization omitted

info=kdbGetInfo(handle);
printf("The library version I'm using is %s\n",info->version);

kdbFreeInfo(info);
 * @endcode
 *
 * @return 0 on sucess, -1 if @p info is NULL, -2 if incompatible app version
 * @see kdbInfoToString(), kdbFreeInfo(), commandInfo()
 * @ingroup kdb
 */
KDBInfo *kdbGetInfo(KDBHandle handle) {
	KDBInfo *info=0;

	info=malloc(sizeof(struct _KDBInfo));
	memset(info,0,sizeof(struct _KDBInfo));

#ifdef HAVE_CONFIG_H
	info->version=VERSION;
#endif

	if (handle) {
		info->backendName=handle->name;
		info->backendIsOpen=1;
	} else {
		info->backendName=getenv("KDB_BACKEND");
		if (!info->backendName) info->backendName="default";

		info->backendIsOpen=0;
	}

	return info;
}



/**
 * Frees the object returned by kdbGetInfo().
 * This method is provided so the programmer doesn't need to learn about the
 * storage internals of the KDBInfo structure.
 *
 * @param info the structure returned by kdbGetInfo()
 * @see kdbGetInfo(), kdbInfoToString(), commandInfo()
 * @ingroup kdb
 *
 */
void kdbFreeInfo(KDBInfo *info) {
	free(info);
	info=0;
}





/**
 * Set some backend-specific @p data in the @p handle.
 *
 * This is useful when your backend have a backend-global context or
 * environment.
 *
 * @param data a pointer to general data specific to a backend implementation.
 * @see kdbhGetBackendData()
 * @ingroup backend
 */
void *kdbhSetBackendData(KDBHandle handle, void *data) {
	return handle->backendData = data;
}


/**
 * Get the previously set backend-specific @p data from the @p handle.
 *
 * This is useful when your backend have a backend-global context or
 * environment.
 * 
 * This method will probably be called everytime one of your kdb*()
 * implementations is called. And if you change something inside the data, you
 * don't have to kdbhSetBackendData() again, bacause you are manipulating your
 * data, and not a copy of it.
 *
 * @par Example:
 * @code
struct MyBackendData {
 int context1;
 int context2;
};

int kdbOpen_mybackend(KDBHandle *handle) {
	struct MyBackendData *context;

	context=malloc(sizeof(struct MyBackendData));
 
	// a random initialization...
	context->context1=1;
	context->context2=2;

	kdbhSetBackendData(*handle,context);

	return 0;
}

int kdbGetKey_maybackend(KDBHandle handle) {
	struct MyBackendData *context;

	context=kdbhGetBackendData(handle);

	// No do something with the context
	. . .

	return 0;
}
 * @endcode
 *
 * On the kdbClose() implementation of your backend, you must remember to
 * free all resources associated to your data. 
 *
 * @par Example of kdbClose() implementation that correctly cleans the context:
 * @code
int kdbClose_mybackend(KDBHandle &handle) {
	struct MyBackendData *context;

	context=kdbhGetBackendData(handle);
	free(context);

	return 0;
}
 * @endcode
 * @return a pointer to the data previously set be kdbhSetBackendData()
 * @ingroup backend
 */
void *kdbhGetBackendData(const KDBHandle handle) {
	return handle->backendData;
}

/**
 * @return the proccess ID of the client application using the @p handle
 * @ingroup backend
 */
pid_t kdbhGetPID(const KDBHandle handle) {
	return handle->pid;
}

/**
 * Set @p handle's internal pid to @p pid.
 * @return the proccess ID of the client application using the @p handle
 * @ingroup backend
 */
pid_t kdbhSetPID(KDBHandle handle,pid_t pid) {
	return handle->pid=pid;
}

/**
 * @return the thread ID of the client application using the @p handle
 * @ingroup backend
 */
pthread_t kdbhGetTID(const KDBHandle handle) {
	return handle->tid;
}

/**
 * Set @p handle's internal tid to @p tid.
 * @return the thread ID of the client application using the @p handle
 * @ingroup backend
 */
pthread_t kdbhSetTID(KDBHandle handle,pthread_t tid) {
	return handle->tid=tid;
}


/**
 * @return the user ID of the client application using the @p handle
 * @ingroup backend
 */
uid_t kdbhGetUID(const KDBHandle handle) {
	return handle->uid;
}

/**
 * Set @p handle's internal uid to @p uid.
 * @return the user ID of the client application using the @p handle
 * @ingroup backend
 */
uid_t kdbhSetUID(KDBHandle handle,uid_t uid) {
	return handle->uid=uid;
}

/**
 * @return the group ID of the client application using the @p handle
 * @ingroup backend
 */
gid_t kdbhGetGID(const KDBHandle handle) {
	return handle->gid;
}

/**
 * Set @p handle's internal gid to @p gid.
 * @return the group ID of the client application using the @p handle
 * @ingroup backend
 */
gid_t kdbhSetGID(KDBHandle handle,gid_t gid) {
	return handle->gid=gid;
}

/**
 * @return the default umask() of the client application using the @p handle
 * @ingroup backend
 */
mode_t kdbhGetUMask(const KDBHandle handle) {
	return handle->umask;
}

/**
 * Set @p handle's internal umask to @p umask.
 * @return the default umask() of the client application using the @p handle
 * @ingroup backend
 */
mode_t kdbhSetUMask(KDBHandle handle,mode_t umask) {
	return handle->umask=umask;
}

/**
 * @return the user name of the client application using the @p handle.
 *         Remember that on some systems many different user names can have same UID.
 * @ingroup backend
 */
char *kdbhGetUserName(const KDBHandle handle) {
	return handle->userName;
}

/**
 * Set @p handle's internal user name to @p userName.
 * @return the user name of the client application using the @p handle.
 *         Remember that on some systems many different user names can have same UID.
 * @ingroup backend
 */
char *kdbhSetUserName(KDBHandle handle,char *userName) {
	char *tmp;
	size_t size;
	
	if ( userName ) {
		size = strblen(userName);
		tmp = realloc(handle->userName, size);
		if ( tmp ) {
			handle->userName = tmp;
			memcpy(handle->userName, userName, size);
		}
	} else {
		free(handle->userName);
		handle->userName = NULL;
	}
	
	return handle->userName;
}

/**
 * Set @p handle's internal backend name to @p backendName.
 * This method should not be used by regular applications.
 * @return @p backendName after being set in @p handle.
 * @ingroup backend
 */
char *kdbhSetBackendName(KDBHandle handle,char *backendName) {
	char *tmp;
	size_t size;
	
	if ( backendName ) {
		size = strblen(backendName);
		tmp = realloc(handle->name, size);
		if ( tmp ) {
			handle->name = tmp;
			memcpy(handle->name, backendName, size);
		}
	} else {
		free(handle->name);
		handle->name = NULL;
	}
			
	return handle->name;
}

/**
 * @return The backend name set in @p handle.
 * @ingroup backend
 */
char *kdbhGetBackendName(KDBHandle handle) {
	return handle->name;
}

/**
 * Convenience method to provide a human readable text for what kdbGetInfo()
 * returns.
 *
 * It is your responsability to allocate and free the @p string buffer.
 * Currently, 200 bytes is a good size for a buffer.
 *
 * @par Example:
 * @code
KDBInfo *info=0;
char buffer[200];

// key dababase initialization omitted

info=kdbGetInfo(handle);
kdbInfoToString(info,buffer,sizeof(buffer));
printf("Follows some information about Elektra:\n");
printf(buffer);
printf("\n");

kdbFreeInfo(info);
 * @endcode
 * @param info the object returned by kdbGetInfo()
 * @param string a pre-allocated buffer to fill with human readable information
 * @param maxSize the size of the string buffer, to avoid memory problems
 * @return 0 on success, -1 if @p info is NULL
 * @see kdbGetInfo(), kdbFreeInfo(), commandInfo()
 * @ingroup kdb
 */
int kdbInfoToString(KDBInfo *info,char *string,size_t maxSize) {
	if (!info) {
		strncpy(string,"No info",maxSize);
		return -1;
	}

	snprintf(string,maxSize,
		"Elektra version: %s\nBackend name: %s\nBackend open: %s",
		info->version,
		info->backendName,
		info->backendIsOpen?"yes":"no");

	return 0;
}



/**
 * This function must be called by a backend's kdbBackendFactory() to
 * define the backend's methods that will be exported. Its job is to
 * organize a libelektra.so's table of virtual methods with pointers to backend
 * dependent methods.
 * 
 * The order and number of arguments are flexible (as keyNew()) to let
 * libelektra.so evolve without breaking its ABI compatibility with backends.
 * So for each method a backend must export, there is a flag defined by
 * #KDBBackendMethod. Each flag tells kdbBackendExport() which method comes
 * next. A backend can have no implementation for a few methods that have
 * default inefficient high-level implementations -- kdbSetKeys(),
 * kdbMonitorKey(), kdbMonitorKeys() -- and to use these defaults, simply
 * don't pass anything to kdbBackendExport() about them.
 * 
 * The last parameter must be @c KDB_BE_END .
 * 
 * @par Example of a complete backend:
 * @code
//
// This is my implementation for an Elektra backend storage.
//
// To compile it:
// $ cc -fpic -o myback.o -c myback.c
// $ cc -shared -fpic -o libelektra-myback.so myback.o
// 
// To use it:
// $ export KDB_BACKEND=myback
// $ kdb ls -Rv
//

#include <kdbbackend.h>

#define BACKENDNAME "my_elektra_backend_implementation"


int kdbOpen_backend(KDBHandle *handle) {...}
int kdbClose_backend(KDBHandle *handle) {...}
int kdbGetKey_backend(KDBHandle handle, Key *key) {...}
int kdbSetKey_backend(KDBHandle handle, Key *key) {...}

... etc implementations of other methods ...



KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,          &kdbOpen_backend,
		KDB_BE_CLOSE,         &kdbClose_backend,
		KDB_BE_GETKEY,        &kdbGetKey_backend,
		KDB_BE_SETKEY,        &kdbSetKey_backend,
		KDB_BE_STATKEY,       &kdbStatKey_backend,
		KDB_BE_RENAME,        &kdbRename_backend,
		KDB_BE_REMOVEKEY,     &kdbRemoveKey_backend,
		KDB_BE_GETCHILD,      &kdbGetKeyChildKeys_backend,
		KDB_BE_SETKEYS,       &kdbSetKeys_backend,
		KDB_BE_MONITORKEY,    &kdbMonitorKey_backend,
		KDB_BE_MONITORKEYS,   &kdbMonitorKeys_backend,
		KDB_BE_END);
}
 * @endcode
 *
 * In the example, the *_backend() methods can have other random names,
 * since you'll correctly pass them later to kdbBackendExport().
 * 
 * @param backendName a simple name for this backend
 * @return an object that contains all backend informations needed by
 * 	libelektra.so
 * @ingroup backend
 */
KDBBackend *kdbBackendExport(const char *backendName, ...) {
	va_list va;
	KDBBackend *returned;
	uint32_t method=0;

	if (backendName == 0) return 0;

	returned=malloc(sizeof(KDBBackend));
	memset(returned,0,sizeof(KDBBackend));
	
	returned->name=(char *)malloc(strblen(backendName));
	strcpy(returned->name,backendName);
	
	/* Start processing parameters */
	
	va_start(va,backendName);

	while ((method=va_arg(va,uint32_t))) {
		switch (method) {
			case KDB_BE_OPEN:
				returned->kdbOpen=va_arg(va,kdbOpenPtr);
				break;
			case KDB_BE_CLOSE:
				returned->kdbClose=va_arg(va,kdbClosePtr);
				break;
			case KDB_BE_STATKEY:
				returned->kdbStatKey=va_arg(va,kdbStatKeyPtr);
				break;
			case KDB_BE_GETKEY:
				returned->kdbGetKey=va_arg(va,kdbGetKeyPtr);
				break;
			case KDB_BE_SETKEY:
				returned->kdbSetKey=va_arg(va,kdbSetKeyPtr);
				break;
			case KDB_BE_RENAME:
				returned->kdbRename=va_arg(va,kdbRenamePtr);
				break;
			case KDB_BE_REMOVEKEY:
				returned->kdbRemoveKey=va_arg(va,kdbRemoveKeyPtr);
				break;
			case KDB_BE_GETCHILD:
				returned->kdbGetKeyChildKeys=
					va_arg(va,kdbGetChildKeysPtr);
				break;
			case KDB_BE_SETKEYS:
				returned->kdbSetKeys=va_arg(va,kdbSetKeysPtr);
				break;
			case KDB_BE_MONITORKEY:
				returned->kdbMonitorKey=
					va_arg(va,kdbMonitorKeyPtr);
				break;
			case KDB_BE_MONITORKEYS:
				returned->kdbMonitorKeys=
					va_arg(va,kdbMonitorKeysPtr);
				break;
		}
	}
	va_end(va);
	
	return returned;
}

/**
 * Resolve key name.
 *
 * This function resolve all key link contained in the path
 * of the supplied key and give the real name.
 *
 * @param handle KDBHandle
 * @param key Key to resolve
 * @param resolvedKeyName Pointer where the full resolved name will be set
 * remember to free it.
 *
 * @return 0 if succeed, -1 otherwise
 *
 */
static int kdbResolveKey(KDBHandle handle, const Key *key, char **resolvedKeyName) {
	Key     *resolvedKey;
	char    *kName, *tmp, *ptr;
	size_t  size;
	int     ret;
	
	resolvedKey = keyNew(KEY_SWITCH_END);
	if ( resolvedKey == NULL ) return -1;
	
	size = keyGetFullNameSize(key);
	kName = (char *) malloc(size);
	tmp = (char *) malloc(size);
	if ( (kName == NULL) || (tmp == NULL) ) {
		keyDel(resolvedKey);
		free(tmp);
		free(kName);
		return -1;
	}
	
	if ( keyGetFullName(key, tmp, size) == -1 ) {
		keyDel(resolvedKey);
		free(tmp);
		free(kName);
		return -1;
	}
	
	size = 0;
	ptr = tmp;
	while ( *(ptr = keyNameGetOneLevel(ptr+size, &size)) != 0 ) {
		strncpy(kName, ptr, size);
		kName[size] = '\0';
		
		if ( keyAddBaseName(resolvedKey, kName) == -1 ) {
			/* Probably not enought memory ... */
			keyDel(resolvedKey);
			free(tmp);
			free(kName);
			return -1;
		}
		
		/* Resolve key link .. */
		for(;;) {
			ret = kdbStatKey(handle, resolvedKey);
			if ( (ret == 0) && !keyIsLink(resolvedKey) ) {
				/* Key isn't a link, stop resolution. */
				break;
				
			} else if ( (ret == -1) && (errno == KDB_RET_NOTFOUND) ) {
				/* Key isn't existing. Stop resolution */
				break;
				
			} else if ( ret == -1 ) {
				/* kdbStatKey() failed because of an internal
				 * error or access denied. Propagate errno. */
				keyDel(resolvedKey);
				free(tmp);
				free(kName);
				return -1;
			}
			
			/* Key is a link. Target key is contained in the value
			 * of the stated key */
			ret = keySetName(resolvedKey, keyStealValue(resolvedKey));
			if ( ret == 0 ) {
				/* Target key's name isn't valid. Propagate errno.
				 * (NULL target isn't valid too) */
				keyDel(resolvedKey);
				free(tmp);
				free(kName);
				return -1;
			}
			
			/* Perhaps the resolved key is a key link too, so iterate
			 * now ... */
		}
	}
	free(tmp);
	free(kName);
	
	size = keyGetFullNameSize(resolvedKey);
	*resolvedKeyName = (char *) malloc(size);
	if ( *resolvedKeyName == NULL ) {
		keyDel(resolvedKey);
		return -1;
	}
	
	keyGetFullName(resolvedKey, *resolvedKeyName, size);
	keyDel(resolvedKey);
	
	return 0;
}

/***************************************************************************
            localkdb.c  -  Methods for accessing the Key Database
                             -------------------
    begin                : Mon Dec 29 2003
    copyright            : (C) 2003 by Avi Alkalay
    email                : avi@unix.sh
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the implementation of a filesystem backend for the            *
 *   Elektra Project. Each Key is a file in the filesystem.                *
 *   It is as secure as filesystem security. It is as reliable             *
 *   as filesystem. It uses only standards C calls, which makes it         *
 *   usable by very low level or early boot stage software, like           *
 *   /sbin/init.                                                           *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id$
$LastChangedBy$

*/


#include "kdb.h"
#include "kdbbackend.h"
#include "kdbprivate.h"


#include <stdlib.h>
#include <stdarg.h>
#include <dlfcn.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <iconv.h>
#include <locale.h>
#include <langinfo.h>
#include <ctype.h>
#include <string.h>



#ifndef KDB_DEFAULT_BACKEND
#define KDB_DEFAULT_BACKEND "filesys"
#endif

extern int errno;




/**
 * @defgroup kdb KeyDB Class Methods
 * @brief General methods to access the Key database.
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * This is the class that accesses the storage backend. When writing a new
 * backend, these are the methods you'll have to reimplement:
 * kdbOpen(), kdbClose(), kdbGetKey(), kdbSetKey(), kdbStatKey(),
 * kdbGetKeyChildKeys(), kdbRemove(), kdbRename().
 *
 * And methods that is suggested to reimplement (but not needed) if you want
 * them to get the benefits of your new backend: kdbSetKeys(),
 * kdbMonitorKey(), kdbMonitorKeys().
 *
 * The other methods are higher level. They use the above methods to do their
 * job, and generally don't have to be reimplemented for a different backend.
 *
 * Language binding writers should follow the same rules:
 * - You should relay completelly on the backend-dependent methods
 * - You may use or reimplement the second set of methods
 * - You should completelly reimplement in your language the higher
 *   lever methods
 */


/*
 * @defgroup internals Elektra internals
 * @brief These methods are not to be used by your application.
 *
 */



struct _KDBBackend {
	void *dlHandle;
	
	char *name;
	
	/* These are the must-have methods */
	
	int (*kdbOpen)();
	int (*kdbClose)();
	
	int (*kdbGetKey)(Key *);
	int (*kdbSetKey)(Key *);
	int (*kdbStatKey)(Key *);
	int (*kdbRename)(Key *, const char *);
	int (*kdbRemove)(const char *);
	int (*kdbGetKeyChildKeys)(const Key *, KeySet *, unsigned long);

	
	
	/* These are the optional methods */
	
	int (*kdbSetKeys)(KeySet *);
	u_int32_t (*kdbMonitorKey)(Key *, u_int32_t,unsigned long, unsigned);
	u_int32_t (*kdbMonitorKeys)(KeySet *, u_int32_t,unsigned long, unsigned);
};




KDBBackend *backend;



/**
 * Opens the session with the Key database.
 *
 * You should allways call this method before retrieving or commiting any
 * keys to the database. Otherwise, consequences are unpredictable.
 * 
 * kdbOpen() relyies on libkdb.so's backend with kdbOpenBackend().
 * The backend defined by the environment variable @e $KDB_BACKEND will
 * be used. If this var is not set in the environment, the backend defined
 * as default at compilation time will be used.
 *
 * Currently you can have only one backend (and key database session)
 * initialized at a certain time. 
 * 
 * To simply manipulate Key or KeySet objects, you don't need to open the key
 * database before with this method.
 * 
 * @see kdbOpenBackend()
 * @see kdbClose()
 * @ingroup kdb
 */
int kdbOpen() {
	char *backendName=0;
	
	backendName=getenv("KDB_BACKEND");
	if (!backendName) backendName=KDB_DEFAULT_BACKEND;
	
	return kdbOpenBackend(backendName);
}



/**
 * Dynamically load a storage beckend for libkdb.so.
 * 
 * After dynamic loading, the backend will be initialized with its
 * implementation of kdbOpen().
 * @param backendName used to define the module filename as
 * 	libkdb-@p "backendName".so
 * @return 0 on success, or whatever is returned by backend's kdbOpen()
 * @see kdbOpen()
 * @ingroup kdb
 */
int kdbOpenBackend(char *backendName) {
	void *dlhandle=0;
	char backendlib[300];
	KDBBackendFactory kdbBackendNew=0;
	
	backend=0;
	
	/* load the environment and make us aware of codeset conversions */
	setlocale(LC_ALL,"");
	
	sprintf(backendlib,"libkdb-%s.so",backendName);
	dlhandle=dlopen(backendlib,RTLD_LAZY);
	if (dlhandle == 0) {
		fprintf(stderr, "libkdb: Could not open \"%s\" backend: %s\n",
			backendName,dlerror());
		return 1; /* error */
	}
	
	kdbBackendNew=dlsym(dlhandle,"kdbBackendFactory");
	if (kdbBackendNew == 0) {
		fprintf(stderr, "libkdb: \"%s\" backend: %s\n",backendName,dlerror());
		return 1; /* error */
	}
	
	backend=(*kdbBackendNew)();
	if (backend == 0) {
		fprintf(stderr,"libkdb: Can't initialize \"%s\" backend\n",
			backendName);
		return 1; /* error */
	}

	/* save the handle for future use */
	backend->dlHandle=dlhandle;
	
	/* let the backend initialize itself */
	return backend->kdbOpen();
}
 



/**
 * Closes the session with the Key database.
 *
 * You should call this method when you finished your affairs with the key
 * database. You can manipulate Key and KeySet objects after kdbClose().
 *
 * This is the counterpart of kdbOpen().
 * @see kdbOpen()
 * @ingroup kdb
 */
int kdbClose() {
	int rc=backend->kdbClose();
	
	if (rc == 0) {
		if (backend->name) free(backend->name);
		dlclose(backend->dlHandle);
		free(backend);
	}
	
	return rc;
}




/**
 * Unencodes a buffer of hexadecimal values. This is a helper function.
 *
 * The allowed format for the hexadecimal values is just
 * a stream of pairs of plain hex-digits, all together or
 * space-separated.
 * @param encoded the source of ASCII hexadecimal digits.
 * @param returned the destination for the unencoded data.
 * @return the amount of bytes unencoded.
 * @ingroup backend
 */
size_t unencode(char *encoded,void *returned) {
	char byteInHexa[5]="0x";
	char *readCursor=encoded;
	char *writeCursor=returned;

	if (!encoded) {
		if (returned) *(char *)returned=0;
		return 0;
	}

	byteInHexa[4]=0;
	while (*readCursor) {
		if (isspace((int)*readCursor)) 
		{
		readCursor++;
		continue;
		}
		if (isxdigit((int)*readCursor)) {
			long int converted;
			byteInHexa[2]=readCursor[0];
			byteInHexa[3]=readCursor[1];
			converted=strtol(byteInHexa,0,16); /* convert from hexa to a byte */
			*writeCursor=(unsigned char)converted;

			readCursor+=2;
			writeCursor++;
		} else {
			/* This is suposed to be a hex-digit stream. But is not, so return. */
			errno=KDB_RET_TYPEMISMATCH;
			return 0;
		}
	}
	return (long int)writeCursor-(long int)returned;
}

/**
 * Checks if UTF-8 conversion is needed in current context.
 *
 * @return 0 if not needed, anything else if needed
 * @ingroup backend
 */
int kdbNeedsUTF8Conversion() {
	return strcmp(nl_langinfo(CODESET),"UTF-8");
}


/**
 * Converts string to (direction=UTF8_TO) and from
 * (direction=UTF8_FROM) UTF-8.
 * 
 * Since Elektra provides portability key values between different codesets,
 * use this helper in your backend to convert the to universal UTF-8
 * strings when storing key names, values and comments.
 *
 * @return 0 on success, -1 otherwise, and propagate @c errno
 * @ingroup backend
 *
 */
int UTF8Engine(int direction, char **string, size_t *inputByteSize) {
	char *currentCharset=0;
	char *converted=0;
	char *readCursor, *writeCursor;
	size_t bufferSize;
	iconv_t converter;

	if (kdbNeedsUTF8Conversion()) currentCharset=nl_langinfo(CODESET);
	else return 0;

	if (direction==UTF8_TO) converter=iconv_open("UTF-8",currentCharset);
	else converter=iconv_open(currentCharset,"UTF-8");

	if (converter == (iconv_t)(-1)) return -1;

	/* work with worst case, when all chars are wide */
	bufferSize=*inputByteSize * 4;
	converted=malloc(bufferSize);
	if (!converted) return -1;

	readCursor=*string;
	writeCursor=converted;
	if (iconv(converter,
			&readCursor,inputByteSize,
			&writeCursor,&bufferSize) == (size_t)(-1)) {
		free(converted);
		iconv_close(converter);
		return -1;
	}

	/* calculate the UTF-8 string byte size, that will be returned */
	*inputByteSize=writeCursor-converted;
	/* store the current unencoded string for future free */
	readCursor=*string;
	/* allocate an optimal size area to store the converted string */
	*string=malloc(*inputByteSize);
	/* copy all that matters for returning */
	memcpy(*string,converted,*inputByteSize);
	/* release memory used by passed string */
	free(readCursor);
	/* release buffer memory */
	free(converted);
	/* release the conversor engine */
	iconv_close(converter);

	return 0;
}



/**
 * Encodes a buffer of data onto hexadecimal ASCII. This is a helper function.
 *
 * The resulting data is made up of pairs of ASCII hex-digits,
 * space- and newline-separated. This is the counterpart of
 * unencode().
 *
 * @param unencoded the source buffer.
 * @param size the size of the source buffer in bytes.
 * @param returned the destination for the ASCII-encoded data.
 * @return the amount of bytes used in the resulting encoded buffer.
 * @see unencode()
 * @ingroup backend
 */
size_t encode(void *unencoded, size_t size, char *returned) {
	void *readCursor=unencoded;
	void *writeCursor=returned;
	int blockStep=4; /* 4 bytes per block */
	int lineStep=8*blockStep; /* 8 blocks per line */
	int currentInBlock=0;
	int currentInLine=0;

	while ((readCursor-unencoded)<size) {
		sprintf(writeCursor,"%02x",*(u_int8_t *)readCursor);
		readCursor++;
		writeCursor+=2;
		currentInBlock++;
		currentInLine++;
		if (currentInLine==lineStep) {
			*(char *)writeCursor='\n'; writeCursor++;
			currentInLine=0;
			currentInBlock=0;
		}
		if (currentInBlock==blockStep) {
			*(char *)writeCursor=' '; writeCursor++;
			currentInBlock=0;
		}
	}
	*(char *)writeCursor='\n';
	*(char *)++writeCursor=0;
	return (writeCursor)-(void *)returned;
}





/**
 * A high-level method to get a key value, by key name.
 * This method is valid only for string keys.
 * You should use other methods to get non-string keys.
 *
 * @see kdbSetValue()
 * @see kdbGetKey()
 * @see kdbGetValueByParent()
 * @see keyGetString()
 * @return 0 on success, or other value and @c errno is set
 * @param keyname the name of the key to receive the value
 * @param returned a buffer to put the key value
 * @param maxSize the size of the buffer
 * @ingroup kdb
 *
 */
int kdbGetValue(const char *keyname, char *returned,size_t maxSize) {
	Key *key;
	int rc=0;

	key=keyNew(keyname,KEY_SWITCH_END);
	rc=kdbGetKey(key);
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
 * @see kdbGetValue()
 * @see keySetString()
 * @see kdbSetKey()
 * @param keyname the name of the key to receive the value
 * @param value the value to be set
 * @return 0 on success, other value otherwise, and @c errno is set
 * @ingroup kdb
 */
int kdbSetValue(const char *keyname, const char *value) {
	Key *key;
	int rc;

/* TODO: check key type first */
	key=keyNew(keyname,KEY_SWITCH_END);
	rc=kdbGetKey(key);
	keySetString(key,value);
	rc=kdbSetKey(key);
	keyDel(key);
	return rc;
}



/**
 * Fills the \p returned buffer with the value of a key, which name
 * is the concatenation of \p parentName and \p baseName.
 *
 * @par Example:
 * @code
char *parent="user/sw/MyApp";
char *keys[]={"key1","key2","key3"};
char buffer[150];   // a big buffer
int c;

for (c=0; c<3; c++) {
	kdbGetValueByParent(parent,keys[c],buffer,sizeof(buffer));
	// Do something with buffer....
}

 * @endcode
 *
 * @see kdbGetKeyByParent()
 * @param parentName the name of the parent key
 * @param baseName the name of the child key
 * @param returned pre-allocated buffer to be filled with key value
 * @param maxSize size of the \p returned buffer
 * @return whathever is returned by kdbGetValue()
 * @ingroup kdb
 */
int kdbGetValueByParent(const char *parentName, const char *baseName, char *returned, size_t maxSize) {
	char name[strblen(parentName)+strblen(baseName)];

	sprintf(name,"%s/%s",parentName,baseName);
	return kdbGetValue(name,returned,maxSize);
}



/**
 * Sets the provided @p value to the key whose name is the concatenation of
 * @p parentName and @p baseName.
 *
 * @param parentName the name of the parent key
 * @param baseName the name of the child key
 * @param value the value to set
 * @ingroup kdb
 */
int kdbSetValueByParent(const char *parentName, const char *baseName, const char *value) {
	char name[strblen(parentName)+strblen(baseName)];

	sprintf(name,"%s/%s",parentName,baseName);
	return kdbSetValue(name,value);
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
 * @return 0 on success, or what kdbGetKey() returns, and @c errno is set
 * @see kdbGetKey()
 * @see kdbGetValueByParent()
 * @see kdbGetKeyByParentKey()
 * @ingroup kdb
 */
int kdbGetKeyByParent(const char *parentName, const char *baseName, Key *returned) {
	char name[strblen(parentName)+strblen(baseName)];

	sprintf(name,"%s/%s",parentName,baseName);
	keySetName(returned,name);

	return kdbGetKey(returned);
}


/**
 * Similar to previous, provided for convenience.
 * @param parent pointer to the parent key
 * @see kdbGetKey()
 * @see kdbGetKeyByParent()
 * @see kdbGetValueByParent()
 * @return 0 on success, or what kdbGetKey() returns, and @c errno is set
 * @ingroup kdb
 */
int kdbGetKeyByParentKey(const Key *parent, const char *baseName, Key *returned) {
	size_t size=keyGetFullNameSize(parent);
	char name[size+strblen(baseName)];

	keyGetFullName(parent,name,size);
	name[size-1]='/';
	strcpy((char *)(name+size),baseName);

	keySetName(returned,name);

	return kdbGetKey(returned);
}





/**
 * Retrieve a number of inter-related keys in one shot.
 * This is one of the most practicall methods of the library.
 * Returns a KeySet with all retrieved keys. So if your application keys
 * live bellow @p system/sw/myApp, you'll use this method to get them all.
 *
 * Option can be any of the following, ORed:
 * - @p KDB_O_RECURSIVE \n
 *   Retrieve also the keys under the child keys, recursively.
 *   The kdb(1) ls command, with switch -R uses this option.
 * - @p KDB_O_DIR \n
 *   By default, folder keys will not be returned because they don't have
 *   values and exist only to define hierarchy. Use this option if you need
 *   them to be included in the returned KeySet.
 * - @p KDB_O_NOVALUE \n
 *   Do not include in @p returned the regular value keys. The resulting KeySet
 *   will be only the skeleton of the tree.
 * - @p KDB_O_STATONLY \n
 *   Only stat(2) the keys; do not retrieve the value, comment and key data
 *   type. The resulting keys will be empty and usefull only for
 *   informational purposes. The kdb(1) ls command, without the -v switch
 *   uses this option.
 * - @p KDB_O_INACTIVE \n
 *   Will make it not ignore inactive keys. So @p returned will be filled also
 *   with inactive keys. See elektra(7) to understand how inactive keys work.
 * - @p KDB_O_SORT \n
 *   Will sort keys alphabetically by their names.
 *
 * @par Example:
 * @code
char errormsg[300];
KeySet *myConfig;
Key *key;

key=keyNew("system/sw/MyApp",KEY_SWITCH_END);
myConfig=ksNew();

kdbOpen();
rc=kdbGetKeyChildKeys(key, myConfig, KDB_O_RECURSIVE);
keyDel(key); // free this resource.... we'll use it later
kdbClose();

// Check and handle propagated error
if (rc) switch (errno) {
	case KDB_RET_INVALIDKEY:
		sprintf(errormsg,"Something invalid");
		perror(errormsg); // use system error messages
		break;
	case KDB_RET_NOTFOUND:
		fprintf(stderr,"Key not found"); // custom error message
		break;
	default:
		sprintf(errormsg,"My application");
		perror(errormsg); // use system error messages
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
 * @param parentName name of the parent key
 * @param returned the (pre-initialized) KeySet returned with all keys found
 * @param options ORed options to control approaches
 * @see #KDBOptions
 * @see kdbGetChildKeys() for a convenience method
 * @see ksLookupByName(), ksLookupRE(), ksLookupByValue()
 * @see ksSort()
 * @see commandList() code in kdb command for usage example
 * @see commandEdit() code in kdb command for usage example
 * @see commandExport() code in kdb command for usage example
 * @return 0 on success, other value on error and @c errno is set
 * @ingroup kdb
 *
 */
int kdbGetKeyChildKeys(const Key *parentKey, KeySet *returned, unsigned long options) {
	return backend->kdbGetKeyChildKeys(parentKey,returned,options);
}



/**
 * This method is similar and calls kdbGetKeyChildKeys(). It is provided for
 * convenience.
 * @ingroup kdb
 */
int kdbGetChildKeys(const char *parentName, KeySet *returned, unsigned long options) {
	Key *parentKey;
	int rc;
	
	parentKey=keyNew(parentName,KEY_SWITCH_END);
	rc=kdbGetKeyChildKeys(parentKey,returned,options);
	
	keyDel(parentKey);
	
	return rc;
}



/**
 * Returns a KeySet with all root keys currently recognized and present
 * on the system. Currently, the @p system and current user's @p user keys
 * are returned.
 * @param returned the initialized KeySet to be filled
 * @return 0
 * @see #KeyNamespace
 * @see commandList() code in kdb command for usage example
 * @ingroup kdb
 *
 */
int kdbGetRootKeys(KeySet *returned) {
	Key *system=0,*user=0;

	system=keyNew("system",KEY_SWITCH_END);
	if (kdbGetKey(system)) {
		keyDel(system);
		system=0;
	}

	user=keyNew("user",KEY_SWITCH_END);
	if (kdbGetKey(user)) {
		keyDel(user);
		user=0;
	}

	if (system) ksInsert(returned,system);
	if (user) ksAppend(returned,user);

	return 0;
}



/**
 * Retrieves only the meta-info of a key from backend storage.
 * The bahavior may change from backend to backend. In the filesystem
 * backend, it will make only a stat to the key.
 *
 * Info like comments and key data type are not retrieved.
 *
 * @param key an initialized Key pointer to be filled.
 * @return 0 on success, -1 otherwise
 * @ingroup kdb
 */
int kdbStatKey(Key *key) {
	return backend->kdbStatKey(key);
}



/**
 * Fully retrieves the passed @p key from the backend storage.
 * 
 * @param key a pointer to a Key that has a name set
 * @return 0 on success, or other value and @c errno is set
 * @see kdbSetKey()
 * @see commandGet() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbGetKey(Key *key) {
	return backend->kdbGetKey(key);
}



/**
 * Commits the @p ks KeySet to the backend storage, starting from @p ks's
 * current cursor until its end. This is why it is suggested that you call
 * ksRewind() on @p ks beffore calling this method.
 * Each key is checked with keyNeedsSync() before being actually commited. So
 * only changed keys are updated.
 *
 * If some error occurs, kdbSetKeys() stops and returns whatever kdbSetKey()
 * returned. The KeySet internal cursor is left on the key that generated
 * the error (so you may check it latter with ksCurrent()). The internal 
 * kdbSetKey() also sets @c errno in case of error.
 *
 * @param ks a KeySet full of changed keys
 * @return 0 on success, or whatever kdbSetKey() returns
 * @see kdbSetKey()
 * @see keyNeedsSync()
 * @see ksNext()
 * @see ksCurrent()
 * @see commandEdit(), commandImport() code in kdb command for usage and error
 *       handling example
 * @ingroup kdb
 */
int kdbSetKeys(KeySet *ks) {
	return backend->kdbSetKeys(ks);
}



int kdbSetKeys_default(KeySet *ks) {
	Key *current=ksCurrent(ks);
	int ret;

	if (!current) current=ksNext(ks);
	while (current) {
		if (keyNeedsSync(current))
			if ((ret=kdbSetKey(current))) /* check error */
				return ret;
		
		current=ksNext(ks);
	}

	return 0;
}



/**
 * Commits a key to the backend storage.
 * If failed (see return), the @c errno global is set accordingly.
 *
 * @see kdbGetKey()
 * @see kdbSetKeys()
 * @see commandSet() code in kdb command for usage example
 * @return 0 on success, or other value and @c errno is set
 * @ingroup kdb
 */
int kdbSetKey(Key *key) {
	return backend->kdbSetKey(key);
}




/**
 * Rename a key in the backend storage.
 *
 * @param key the key to be renamed
 * @param newName the new key name
 * @return 0 on success, -1 or whathever is returned by rename() on failure,
 * 	and @c errno is propagated
 * @ingroup kdb
 */
int kdbRename(Key *key, const char *newName) {
	return backend->kdbRename(key,newName);
}





/**
 * Remove a key from the backend storage.
 * This method is not recursive.
 *
 * @param keyName the name of the key to be removed
 * @return whathever is returned by remove(), and @c errno is propagated
 * @see commandRemove() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbRemove(const char *keyName) {
	return backend->kdbRemove(keyName);
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
int kdbLink(const char *oldPath, const char *newKeyName) {
	Key *key;
	int rc;

	key=keyNew(newKeyName,KEY_SWITCH_END);
	keySetLink(key,oldPath);

	rc=kdbSetKey(key);
	keyDel(key);

	return rc;
}




/**
 * Monitor a KeySet for some key change.
 *
 * This method will scan the @p interests KeySet, starting and finishing in
 * the KeySet next cursor position, in a circular behavior, looking for some
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

myConfigs=ksNew();
kdbGetChildKeys("system/sw/MyApp",myConfigs,KDB_O_RECURSIVE | KDB_O_SORT);

// use the keys . . . .

// now monitor any key change
ksRewind(myConfigs);
while (1) {
	Key *changed=0;
	char keyName[300];
	char keyData[300];
	u_int32_t diff;

	// block until any change in key value or comment . . .
	diff=kdbMonitorKeys(myConfigs,
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
 * @see kdbMonitorKey()
 * @see ksCurrent()
 * @see ksRewind()
 * @see ksNext()
 * @see KeyFlags
 * @see commandMonitor() code in kdb command for usage example
 * @ingroup kdb
 *
 */
u_int32_t kdbMonitorKeys(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	return backend->kdbMonitorKeys(interests,diffMask,iterations,sleep);
}


u_int32_t kdbMonitorKeys_default(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	Key *start,*current;
	u_int32_t diff;
	int infinitum=0;

	if (!interests || !interests->size) return 0;

	/* Unacceptable 0 usecs sleep. Defaults to 1 second */
	if (!sleep) sleep=1000;

	if (!iterations) infinitum=1;
	else infinitum=0;

	current=start=ksCurrent(interests);

	while (infinitum || --iterations) {
		do {
			diff=kdbMonitorKey(current,diffMask,1,0);
			if (diff) return diff;
			current=ksNext(interests);
		} while (current!=start);

		/* Test if some iterations left . . . */
		if (infinitum || iterations) usleep(sleep);
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
 * some change happens
 * @param sleep time to sleep, in microseconds, between iterations.
 * 0 defaults to 1 second.
 * @return the ORed @p KEY_SWITCH_* flags of what changed
 * @see #KeySwitch
 * @see keyCompare()
 * @see kdbMonitorKeys() to monitor KeySets, and for a code example
 * @see commandMonitor() code in kdb command for usage example
 * @ingroup kdb
 *
 */
u_int32_t kdbMonitorKey(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	return backend->kdbMonitorKey(interest,diffMask,iterations,sleep);
}




u_int32_t kdbMonitorKey_default(Key *interest, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	Key *tested;
	int rc;
	u_int32_t diff;
	int infinitum=0;

	/* consistency */
	if (!interest || !keyGetNameSize(interest)) return 0;

	/* Unacceptable 0 usecs sleep. Defaults to 1 second */
	if (!sleep) sleep=1000;

	if (!iterations) infinitum=1;
	else infinitum=0;

	/* Work with a copy of the key */
	tested=keyNew(0);
	keyDup(interest,tested);

	while (infinitum || --iterations) {
		rc=kdbGetKey(tested);
		if (rc) {
			keyDel(tested);
			/* check what type of problem happened.... */
			switch (errno) {
				case KDB_RET_NOCRED:
					return KEY_SWITCH_NEEDSYNC;
				case KDB_RET_NOTFOUND:
					return KEY_SWITCH_FLAG;
			}
		}
		
		diff=keyCompare(tested,interest);
		
		if (diff & diffMask) {
			/* If differences interests us, return it, otherwise cycle again.
			 * We don't loose the original key context in a KeySet because
			 * we worked with a copy of the key.
			 */
			keyDup(tested,interest);
			keyDel(tested);
			return diff;
		}
		/* Test if some iterations left . . . */
		if (infinitum || iterations) usleep(sleep);
	}
	
	keyDel(tested);

	return 0;
}



/**
 * This function must be called by a backend's kdbBackendFactory() to
 * define the backend's methods that will be exported. Its job is to
 * organize a libkdb.so's internal structure with pointers to backend
 * dependent methods.
 * 
 * The order and number of arguments are flexible (as keyNew()) to let
 * libkdb.so evolve without breaking its ABI compatibility with backends.
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
#include <kdb.h>
#include <kdbbackend.h>

#define BACKENDNAME "my_elektra_backend_implementation"


int kdbOpen_backend() {...}
int kdbClose_backend() {...}
int kdbGetKey_backend() {...}
int kdbSetKey_backend() {...}

... etc implementations of other methods ...



KDBBackend *kdbBackendFactory(void) {
	return kdbBackendExport(BACKENDNAME,
		KDB_BE_OPEN,&kdbOpen_backend,
		KDB_BE_CLOSE,&kdbClose_backend,
		KDB_BE_GETKEY,&kdbGetKey_backend,
		KDB_BE_SETKEY,&kdbSetKey_backend,
		KDB_BE_STATKEY,&kdbStatKey_backend,
		KDB_BE_RENAME,&kdbRename_backend,
		KDB_BE_REMOVE,&kdbRemove_backend,
		KDB_BE_GETCHILD,&kdbGetKeyChildKeys_backend,
		KDB_BE_SETKEYS,&kdbSetKeys_backend,
		KDB_BE_MONITORKEY,&kdbMonitorKey_backend,
		KDB_BE_MONITORKEYS,&kdbMonitorKeys_backend,
		KDB_BE_END);
}
 * @endcode
 *
 * In the example, the *_backend() methods can have other random names,
 * since you'll correctly pass them later to kdbBackendExport().
 * 
 * @param backendName a simple name for this backend
 * @return an object that contains all backend informations needed by
 * 	libkdb.so
 * @ingroup backend
 */
KDBBackend *kdbBackendExport(const char *backendName, ...) {
	va_list va;
	KDBBackend *returned;
	u_int32_t method=0;

	if (backendName == 0) return 0;
	
	returned=malloc(sizeof(KDBBackend));
	memset(returned,0,sizeof(KDBBackend));
	
	returned->name=(char *)malloc(strblen(backendName));
	strcpy(returned->name,backendName);
	
	/* Default inefficient high-level internal implementations */
	returned->kdbSetKeys=&kdbSetKeys_default;
	returned->kdbMonitorKey=&kdbMonitorKey_default;
	returned->kdbMonitorKeys=&kdbMonitorKeys_default;
	
	
	/* Start processing parameters */
	
	va_start(va,backendName);

	while ((method=va_arg(va,u_int32_t))) {
		switch (method) {
			case KDB_BE_OPEN:
				returned->kdbOpen=va_arg(va,int (*)());
				break;
			case KDB_BE_CLOSE:
				returned->kdbClose=va_arg(va,int (*)());
				break;
			case KDB_BE_STATKEY:
				returned->kdbStatKey=va_arg(va,int (*)(Key *));
				break;
			case KDB_BE_GETKEY:
				returned->kdbGetKey=va_arg(va,int (*)(Key *));
				break;
			case KDB_BE_SETKEY:
				returned->kdbSetKey=va_arg(va,int (*)(Key *));
				break;
			case KDB_BE_RENAME:
				returned->kdbRename=va_arg(va,int (*)(Key *, const char *));
				break;
			case KDB_BE_REMOVE:
				returned->kdbRemove=va_arg(va,int (*)(const char *));
				break;
			case KDB_BE_GETCHILD:
				returned->kdbGetKeyChildKeys=
					va_arg(va,int (*)(const Key *, KeySet *, unsigned long));
				break;
			case KDB_BE_SETKEYS:
				returned->kdbSetKeys=va_arg(va,int (*)(KeySet *));
				break;
			case KDB_BE_MONITORKEY:
				returned->kdbMonitorKey=
					va_arg(va,u_int32_t (*)(Key *, u_int32_t,unsigned long, unsigned));
				break;
			case KDB_BE_MONITORKEYS:
				returned->kdbMonitorKeys=
					va_arg(va,u_int32_t (*)(KeySet *, u_int32_t,unsigned long, unsigned));
				break;
		}
	}
	va_end(va);
	
	return returned;
}


/**
 * @mainpage The Elektra Key/Value Pair Database API
 *
 * @section overview Elektra Project Overview
 *
 * Elektra is a project to unify Linux/Unix configurations. It does that
 * providing an hierarchical namespace to store configuration keys and
 * their values, an API to access/modify them, and some command line tools.
 *
 * Everything about the project can be found at http://elektra.sf.net
 *
 * @section classes Elektra API
 *
 * The API was written in pure C because Elektra was designed to be usefull
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
 *   - Retrieve and commit Keys and KeySets, recursively or not
 *   - Retrieve and commit individual Keys value, by absolute name or relative to parent
 *   - Monitor and notify changes in Keys and KeySets
 *   - Create and delete regular, folder or symbolic link Keys
 *   - See @ref kdb "class documentation" for more
 *
 * @subsection Key Key
 *   - Get and Set key properties like name, root and base name, value, type,
 *      permissions, changed time, description, etc
 *   - Compare all properties with other keys
 *   - Test if changed, if is a @p user/ or @p system/ key, etc
 *   - Flag it and test if key has a flag
 *   - Export Keys to an XML representation
 *   - See @ref key "class documentation" for more
 *
 * @subsection KeySet KeySet
 *   - Linked list of Key objects
 *   - Insert and append entire KeySets or Keys
 *   - Work with its internal cursor
 *   - Compare entire KeySets
 *   - Export KeySets to an XML representation
 *   - See @ref keyset "class documentation" for more
 *
 *
 * @section keynames Key Names and Namespaces
 *
 * There are 2 trees of keys: @p system and @p user
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
 * - It is suggested to make your application look for default keys under
 *   @p system/sw/MyApp/current and/or @p user/sw/MyApp/current. This way, from
 *   a sysadmin perspective, it will be possible to copy the
 *   @p system/sw/MyApp/current tree to something like @p system/sw/MyApp/old,
 *   and keep system clean and organized.
 *
 *
 */









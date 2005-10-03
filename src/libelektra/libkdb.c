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
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/




/* Subversion stuff

$Id: libkdb.c 252 2005-07-30 11:58:10Z aviram $
$LastChangedBy: aviram $

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



extern int errno;




/**
 * @defgroup kdb KeyDB :: Class Methods
 * @brief General methods to access the Key database.
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * This is the class that accesses the storage backend. When @link backend writing a new
 * backend @endlink, these are the methods you'll have to reimplement:
 * kdbOpen(), kdbClose(), kdbGetKey(), kdbSetKey(), kdbStatKey(),
 * kdbGetKeyChildKeys(), kdbRemove(), kdbRename().
 *
 * And methods that are suggested to reimplement (but not needed) if you want
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
	int (*kdbRemoveKey)(const Key *);
	int (*kdbGetKeyChildKeys)(const Key *, KeySet *, unsigned long);

	
	
	/* These are the optional methods */
	
	int (*kdbSetKeys)(KeySet *);
	u_int32_t (*kdbMonitorKey)(Key *, u_int32_t,unsigned long, unsigned);
	u_int32_t (*kdbMonitorKeys)(KeySet *, u_int32_t,unsigned long, unsigned);
};




KDBBackend *backend;



/**
 * Opens the session with the Key database, using a backend defined by
 * environment var @e $KDB_BACKEND. If the environment is not set
 * the @e default backend will be opened.
 *
 * You should allways call this method before retrieving or commiting any
 * keys to the database. Otherwise, consequences are unpredictable. And
 * after using the key database, you should not forget to kdbClose().
 *
 * This is the best way to have affairs with the key database, unless
 * the program is concerned about security and authentication (e.g. su,
 * login, telnetd, etc), in which kdbOpenDefault() should be used. kdbOpen()
 * is used by the kdb command.
 *
 * Currently you can have only one backend (and key database session)
 * initialized at a certain time. 
 *
 * To simply manipulate Key or KeySet objects without having to retrieve them
 * from the storage, you don't need to open the key database before with any
 * of the kdbOpen*() methods.
 *
 * @see kdbOpenDefault(), kdbOpenBackend(), kdbClose()
 * @return 0 on success or whatever is returned by kdbOpenBackend()
 * @ingroup kdb
 */
int kdbOpen() {
	char *backendName=0;
	
	backendName=getenv("KDB_BACKEND");
	if (backendName) return kdbOpenBackend(backendName);
	else return kdbOpenBackend("default");
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
 * @see kdbOpen(), kdbOpenBackend(), kdbClose()
 * @return 0 on success or whatever is returned by kdbOpenBackend()
 * @ingroup kdb
 */
int kdbOpenDefault() {
	return kdbOpenBackend("default");
}




/**
 * Opens the session with the Key database, dynamically loading a specific
 * beckend for libelektra.so.
 * 
 * After dynamic loading, the backend will be initialized with its
 * implementation of kdbOpen().
 * 
 * @param backendName used to define the module filename as
 * 	libelektra-@p "backendName".so
 * @return 0 on success. On failure, @c errno is set to KDBErr::KDB_RET_NOSYS
 * 	and 1 if backend library could not be opened, 2 if backend doesn't have
 * 	the essential "kdbBackendFactory" initialization symbol, 3 if backend
 * 	failed to export its methods, 4 if backend does not provide a kdbOpen()
 * 	implementation, or anything else that the backend's kdbOpen implementation (see kdbOpen_backend()) returns.
 * @see kdbOpen()
 * @par Example of copying keys from one backend to another
 * @code
KeySet *ks=ksNew();

kdbOpen(); // open default backend
kdbGetChildKeys("system/sw/httpd",ks, 
	KDB_O_NFOLLOWLINK |  // we want real links, not their targets
	KDB_O_INACTIVE |     // even commented (inactive) keys
	KDB_O_DIR |          // even pure directory keys
	KDB_O_RECURSIVE |    // all of this recursivelly
	KDB_O_SORT);         // sort all
kdbClose();

kdbOpenBackend("apache");

// The hipotethical libelektra-apache.so backend implementation for kdbSetKeys()
// simply interprets the passed KeySet and generates an old style
// equivalent /etc/httpd/httpd.conf file.
kdbSetKeys(ks);
kdbClose();

ksDel(ks);
 * @endcode
 * @par Emulating same bahavior of previous example but now with the kdb command
 * @code
bash# kdb export system/sw/httpd > apacheconf.xml
bash# KDB_BACKEND=apache kdb import apacheconf.xml
 * @endcode
 * @ingroup kdb
 */
int kdbOpenBackend(char *backendName) {
	void *dlhandle=0;
	char backendlib[300];
	KDBBackendFactory kdbBackendNew=0;
	int rc=0;
	
	backend=0;
	
	/* load the environment and make us aware of codeset conversions */
	setlocale(LC_ALL,"");
	
	sprintf(backendlib,"libelektra-%s.so",backendName);
	dlhandle=dlopen(backendlib,RTLD_LAZY);
	if (dlhandle == 0) {
		fprintf(stderr, "libelektra: Could not open \"%s\" backend: %s\n",
			backendName,dlerror());
		errno=KDB_RET_NOSYS;
		return 1; /* error */
	}
	
	kdbBackendNew=dlsym(dlhandle,"kdbBackendFactory");
	if (kdbBackendNew == 0) {
		fprintf(stderr, "libelektra: \"%s\" backend: %s\n",backendName,dlerror());
		errno=KDB_RET_NOSYS;
		return 2; /* error */
	}
	
	backend=(*kdbBackendNew)();
	if (backend == 0) {
		fprintf(stderr,"libelektra: Can't initialize \"%s\" backend\n",
			backendName);
		errno=KDB_RET_NOSYS;
		return 3; /* error */
	}

	/* save the handle for future use */
	backend->dlHandle=dlhandle;
	
	/* let the backend initialize itself */
	if (backend->kdbOpen) rc=backend->kdbOpen();
	else {
		errno=KDB_RET_NOSYS;
		rc=4;
	}
	return rc;
}



/**
 * Closes the session with the Key database.
 *
 * You should call this method when you finished your affairs with the key
 * database. You can manipulate Key and KeySet objects after kdbClose().
 *
 * This is the counterpart of kdbOpen().
 * @see kdbOpen()
 * @return 0 on success, anything else on failure, and @c errno is set.
 * 	If the backend implementation of kdbOpen can't be found, @c errno is
 * 	set to KDBErr::KDB_RET_NOSYS.
 * @ingroup kdb
 */
int kdbClose() {
	int rc=0;
	
	if (backend && backend->kdbClose) rc=backend->kdbClose();
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	if (rc == 0) {
		if (backend->name) free(backend->name);
		dlclose(backend->dlHandle);
		free(backend); backend=0;
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
 * @return the amount of bytes unencoded, or a negative value and @c errno
 * 	is set to KDB_RET_TYPEMISMATCH
 * @ingroup backend
 */
ssize_t unencode(char *encoded,void *returned) {
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
			return -1;
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
 * Converts string to (@p direction = @c UTF8_TO) and from
 * (@p direction = @c UTF8_FROM) UTF-8.
 * 
 * Since Elektra provides portability for key names and string values between
 * different codesets, you should use this helper in your backend to convert
 * to and from universal UTF-8 strings, when storing key names, values and
 * comments.
 *
 * @param direction must be @c UTF8_TO (convert from current non-UTF-8 to
 * 	UTF-8) or @c UTF8_FROM (convert from UTF-8 to current non-UTF-8)
 * @param string before the call: the string to be converted; after the call:
 * 	reallocated to carry the converted string
 * @param inputOutputByteSize before the call: the size of the string including
 * 	leading NULL; after the call: the size of the converted string including
 * 	leading NULL
 * @return 0 on success, -1 otherwise and @c errno is propagated
 * @ingroup backend
 *
 */
int UTF8Engine(int direction, char **string, size_t *inputOutputByteSize) {
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
	bufferSize=*inputOutputByteSize * 4;
	converted=malloc(bufferSize);
	if (!converted) return -1;

	readCursor=*string;
	writeCursor=converted;
	if (iconv(converter,
			&readCursor,inputOutputByteSize,
			&writeCursor,&bufferSize) == (size_t)(-1)) {
		free(converted);
		iconv_close(converter);
		return -1;
	}

	/* calculate the UTF-8 string byte size, that will be returned */
	*inputOutputByteSize=writeCursor-converted;
	/* store the current unencoded string for future free */
	readCursor=*string;
	/* allocate an optimal size area to store the converted string */
	*string=malloc(*inputOutputByteSize);
	/* copy all that matters for returning */
	memcpy(*string,converted,*inputOutputByteSize);
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
ssize_t encode(void *unencoded, size_t size, char *returned) {
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
 * @param keyname the name of the key to receive the value
 * @param returned a buffer to put the key value
 * @param maxSize the size of the buffer
 * @return 0 on success, or other value in case of error, and @c errno is set
 * @see kdbSetValue(), kdbGetKey(), kdbGetValueByParent(), keyGetString()
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
 * @param keyname the name of the key to receive the value
 * @param value the value to be set
 * @return 0 on success, other value otherwise, and @c errno is set
 * @see kdbGetValue(), keySetString(), kdbSetKey()
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
 * Fills the @p returned buffer with the value of a key, which name
 * is the concatenation of @p parentName and @p baseName.
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
 * @param parentName the name of the parent key
 * @param baseName the name of the child key
 * @param returned pre-allocated buffer to be filled with key value
 * @param maxSize size of the \p returned buffer
 * @return whathever is returned by kdbGetValue()
 * @see kdbGetKeyByParent()
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
 * @Return whatever is returned by kdbSetValue()
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
 * @see kdbGetKey(), kdbGetValueByParent(), kdbGetKeyByParentKey()
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
 * @see kdbGetKey(), kdbGetKeyByParent(), kdbGetValueByParent()
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
 * - @p KDBOptions::KDB_O_RECURSIVE \n
 *   Retrieve also the keys under the child keys, recursively.
 *   The kdb(1) ls command, with switch -R uses this option.
 * - @p KDBOptions::KDB_O_DIR \n
 *   By default, folder keys will not be returned because they don't have
 *   values and exist only to define hierarchy. Use this option if you need
 *   them to be included in the returned KeySet.
 * - @p KDBOptions::KDB_O_DIRONLY \n
 *   Include in @p returned only the directory keys. The resulting KeySet
 *   will be only the skeleton of the tree.
 * - @p KDBOptions::KDB_O_STATONLY \n
 *   Only stat(2) the keys; do not retrieve the value, comment and key data
 *   type. The resulting keys will be empty and usefull only for
 *   informational purposes. The kdb(1) ls command, without the -v switch
 *   uses this option.
 * - @p KDBOptions::KDB_O_INACTIVE \n
 *   Will make it not ignore inactive keys. So @p returned will be filled also
 *   with inactive keys. See elektra(7) to understand how inactive keys work.
 * - @p KDBOptions::KDB_O_SORT \n
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
 * @param parentKey parent key
 * @param returned the (pre-initialized) KeySet returned with all keys found
 * @param options ORed options to control approaches
 * @see #KDBOptions
 * @see kdbGetChildKeys() for a convenience method
 * @see ksLookupByName(), ksLookupRE(), ksLookupByValue() for powerfull
 * 	lookups after the KeySet was retrieved
 * @see ksSort() for what is done when you ask for KDBOptions::KDB_O_SORT
 * @see commandList() code in kdb command for usage example
 * @see commandEdit() code in kdb command for usage example
 * @see commandExport() code in kdb command for usage example
 * @return number of keys contained by @p returned, or a negative value on
 * 	error and @c errno is set
 * @ingroup kdb
 *
 */
ssize_t kdbGetKeyChildKeys(const Key *parentKey, KeySet *returned, unsigned long options) {
	
	if (backend && backend->kdbGetKeyChildKeys)
		return backend->kdbGetKeyChildKeys(parentKey,returned,options);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
}



/**
 * This method is similar and calls kdbGetKeyChildKeys(). It is provided for
 * convenience.
 * @ingroup kdb
 */
ssize_t kdbGetChildKeys(const char *parentName, KeySet *returned, unsigned long options) {
	Key *parentKey;
	ssize_t rc;
	
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
 * @return the number of root keys found
 * @see #KeyNamespace
 * @see commandList() code in kdb command for usage example
 * @ingroup kdb
 *
 */
ssize_t kdbGetRootKeys(KeySet *returned) {
	Key *system=0,*user=0;

	user=keyNew("user",KEY_SWITCH_NEEDSYNC,KEY_SWITCH_END);
	if (user->flags & KEY_SWITCH_FLAG) {
		keyDel(user);
		user=0;
	} else ksInsert(returned,user);

	system=keyNew("system",KEY_SWITCH_NEEDSYNC,KEY_SWITCH_END);
	if (system->flags & KEY_SWITCH_FLAG) {
		keyDel(system);
		system=0;
	} else ksInsert(returned,system);

	return returned->size;
}



/**
 * Taps the key only for its meta-info from the backend storage.
 * 
 * The bahavior may change from backend to backend. In the filesystem
 * backend, it will make only a stat(2) on the key.
 * 
 * A key of type KEY_TYPE_LINK will have its target address loaded in the
 * @p key structure, which can be accessed later using keyStealValue() or
 * keyGetString(). This is the only way to know the target of a link key
 * without dereferencing it (in contrast to kdbGetKey(), where the link is
 * dereferenced).
 *
 * Info like comments and key data type are not retrieved.
 *
 * @param key an initialized Key pointer to be filled.
 * @return 0 on success, -1 otherwise
 * @ingroup kdb
 */
int kdbStatKey(Key *key) {
	int rc=0;
	
	if (backend && backend->kdbStatKey)
		rc=backend->kdbStatKey(key);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
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
	int rc=0;
	
	if (backend && backend->kdbGetKey)
		rc=backend->kdbGetKey(key);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}



/**
 * Commits the @p ks KeySet to the backend storage, starting from @p ks's
 * current position until its end. This is why it is suggested that you call
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
 * @see kdbSetKey(), keyNeedsSync(), ksNext(), ksCurrent()
 * @see commandEdit(), commandImport() code in kdb command for usage and error
 *       handling example
 * @ingroup kdb
 */
int kdbSetKeys(KeySet *ks) {
	int rc=0;
	
	if (backend && backend->kdbSetKeys)
		rc=backend->kdbSetKeys(ks);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}



/**
 * A high level, probably inefficient implementation for the kdbSetKeys()
 * method. If a backend doesn't want to reimplement this method, this
 * implementation can be used, in which kdbSetKey() will be called for
 * each Key object contained in @p ks.
 *
 * @see kdbSetKeys(), kdbSetKeys_backend()
 *
 * @ingroup backend
 */
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
 * Sets @p key in the backend storage.
 *
 * @see kdbGetKey(), kdbSetKeys()
 * @see commandSet() code in kdb command for usage example
 * @return 0 on success, or other value and @c errno is set
 * @ingroup kdb
 */
int kdbSetKey(Key *key) {
	int rc=0;
	
	if (backend && backend->kdbSetKey)
		rc=backend->kdbSetKey(key);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}




/**
 * Rename a key in the backend storage.
 *
 * @param key the key to be renamed
 * @param newName the new key name
 * @return 0 on success, or whathever is returned by the backend
 * 	implementation on failure, and @c errno is propagated
 * @ingroup kdb
 */
int kdbRename(Key *key, const char *newName) {
	int rc=0;
	
	if (backend && backend->kdbRename)
		rc=backend->kdbRename(key,newName);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}



/**
 * Remove a key from the backend storage.
 * This method is not recursive.
 *
 * @param key the key to be removed
 * @return 0 on success, or whathever is returned by the backend
 * 	implementation on failure, and @c errno is propagated
 * @see commandRemove(), and ksCompare() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbRemoveKey(const Key *key) {
	int rc=0;
	
	if (backend && backend->kdbRemoveKey)
		rc=backend->kdbRemoveKey(key);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}



/**
 * Remove a key by its name from the backend storage.
 * This is a convenience to kdbRemoveKey().
 *
 * @param keyName the name of the key to be removed
 * @return 0 on success, or whathever is returned by kdbRemoveKey(),
 * 	and @c errno is propagated
 * @see commandRemove() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbRemove(const char *keyName) {
	int rc=0;
	Key *key=0;
	
	key=keyNew(KEY_SWITCH_END);
	rc=keySetName(key,keyName);
	if (rc == 0) {
		keyDel(key);
		return -1; /* error */
	}
	
	rc=kdbRemoveKey(key);
	keyDel(key);
	
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
 * @see kdbMonitorKey(), ksCurrent(), ksRewind(), ksNext(), #KeySwitch
 * @see commandMonitor() code in kdb command for usage example
 * @ingroup kdb
 *
 */
u_int32_t kdbMonitorKeys(KeySet *interests, u_int32_t diffMask,
		unsigned long iterations, unsigned sleep) {
	
	u_int32_t rc=0;
	
	if (backend && backend->kdbMonitorKeys)
		rc=backend->kdbMonitorKeys(interests,diffMask,iterations,sleep);
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
 * @ingroup backend
 */
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
 * 	some change happens
 * @param sleep time to sleep, in microseconds, between iterations.
 * 	0 defaults to 1 second.
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
	
	int rc=0;
	
	if (backend && backend->kdbMonitorKey)
		rc=backend->kdbMonitorKey(interest,diffMask,iterations,sleep);
	else {
		errno=KDB_RET_NOSYS;
		return 0;
	}
	
	return rc;
}




/**
 * A high level, probably inefficient, implementation for the kdbMonitorKey()
 * method. If a backend doesn't want to reimplement this method, this
 * implementation can be used.
 *
 * @ingroup backend
 */
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
			/* check what type of problem happened.... */
			switch (errno) {
				case KDB_RET_NOCRED:
					keyDel(tested);
					return KEY_SWITCH_NEEDSYNC;
				case KDB_RET_NOTFOUND:
					keyDel(tested);
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

#include <kdb.h>
#include <kdbbackend.h>

#define BACKENDNAME "my_elektra_backend_implementation"


int kdbOpen_backend() {...}
int kdbClose_backend() {...}
int kdbGetKey_backend(Key *key) {...}
int kdbSetKey_backend(Key *key) {...}

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
	u_int32_t method=0;

	if (backendName == 0) return 0;
	
	returned=malloc(sizeof(KDBBackend));
	memset(returned,0,sizeof(KDBBackend));
	
	returned->name=(char *)malloc(strblen(backendName));
	strcpy(returned->name,backendName);
	
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
			case KDB_BE_REMOVEKEY:
				returned->kdbRemoveKey=va_arg(va,int (*)(const Key *));
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
 * @mainpage The Elektra API
 *
 * @section overview Elektra Initiative Overview
 *
 * Elektra is an initiative to unify Linux/Unix configurations. It does that
 * providing an hierarchical namespace to store configuration keys and
 * their values, an API to access/modify them, and command line tools.
 *
 * Everything about the initiative can be found at http://elektra.sf.net
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
 *   - @link ksToStream() Export KeySets to an XML representation @endlink
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
 * - It is suggested to make your application look for default keys under
 *   @p system/sw/MyApp/current and/or @p user/sw/MyApp/current. This way, from
 *   a sysadmin perspective, it will be possible to copy the
 *   @p system/sw/MyApp/current tree to something like @p system/sw/MyApp/old,
 *   and keep system clean and organized.
 *
 *
 */









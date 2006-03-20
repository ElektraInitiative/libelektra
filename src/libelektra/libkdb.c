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
 * - @c fstab: a reference backend used to interpret the @c /etc/fstab file as
 *   a set of keys under @c system/filesystems .
 * - @c gconf: makes Elektra use the GConf daemon to access keys. Only the
 *   @c user/ tree is available since GConf is not system wide.
 *
 * Backends are phisically a library with name @c /lib/libelektra-{NAME}.so .
 *
 * In general usage, the @c default backend will be used, which is a pointer to
 * some other backend. Your program can use a different backend simply by
 * setting the @e KDB_BACKEND environment variable. Or, if you know what you
 * are doing, you can hardcode it in your code and use the explicit
 * kdbOpenBackend() method to use one.
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
 * The other methods are higher level. They use the above methods to do their
 * job, and generally don't have to be reimplemented for a different backend.
 *
 * Language binding writers should follow the same rules:
 * - You should relay completelly on the backend-dependent methods
 * - You may use or reimplement the second set of methods
 * - You should completelly reimplement in your language the higher
 *   lever methods
 */




#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdarg.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <stdio.h>
#ifdef HAVE_ICONV
#include <iconv.h>
#endif
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif
#include <ctype.h>
#include <string.h>


/* kdbbackend.h will include kdb.h and kdbprivate.h */
#include "kdbbackend.h"
#include "kdbLibLoader.h"

/* usleep doesn't exist on win32, so we use Sleep() */
#ifdef WIN32
#define usleep(x) Sleep(x)
#endif

/*extern int errno;*/



struct _KDBBackend {
	/* dynamic libloader data */
	kdbLibHandle dlHandle;
	
	/* backend name */
	char *name;
	
	/* backend specific data to carry along kdb*() calls */
	void *backendData;
	
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
 * @return -1 on failure
 * @errno is not set on failure up to now, because there is no backend
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
	KDBBackend *backend=0;
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
	kdbBackendFactory=(KDBBackendFactory)kdbLibSym(dlhandle,
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

	/* save the handle for future use */
	(*handle)->dlHandle=dlhandle;
	
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
 * database. You can manipulate Key and KeySet objects after kdbClose().
 *
 * This is the counterpart of kdbOpen().
 *
 * The @p handle parameter will be finalized and all resources associated to it
 * will be freed. After a kdbClose(), this @p handle can't be used anymore,
 * unless it gets initialized again with another call to kdbOpen().
 *
 * @param handle the key database handler to initialize
 * @see kdbOpen()
 * @return 0 on success, anything else on failure, and @c errno is set.
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
		if ((*handle)->name) free((*handle)->name);
		kdbLibClose((*handle)->dlHandle);
		free(*handle); *handle=0;
	}
	
	return rc;
}




/**
 * Unencodes a buffer of ASCII hexadecimal values into a byte stream.
 *
 * The allowed format for the hexadecimal values is just
 * a stream of pairs of plain hex-digits, all together or
 * space-separated.
 * 
 * The @c returned data won't be bigger than half the size of the
 * source @c encoded data.
 *
 * @param encoded the source of ASCII hexadecimal digits.
 * @param returned preallocated destination for the unencoded data.
 * @return the amount of bytes unencoded, or a negative value and @c errno
 * 	is set to KDB_RET_TYPEMISMATCH
 * @see encode()
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
 * if nl_langinfo() is not available, no conversion is ever needed.
 * If iconv usage is disabled there is no need to check if we need to convert.
 * Furthermore, some systems have nl_langinfo(), but lacks ability to get
 * CODESET through it.
 * Look at the comments by the UTF8Engine() function for more information.
 *
 * @return 0 if not needed, anything else if needed
 * @ingroup backend
 */
int kdbNeedsUTF8Conversion() {
#if defined(HAVE_NL_LANGINFO) && defined(HAVE_ICONV) && defined(CODESET)
	return strcmp(nl_langinfo(CODESET),"UTF-8");
#else
	return 0;	  
#endif
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
 * If iconv() or nl_langinfo() is not available on your system, or if iconv()
 * usage is disabled (--disable-iconv on build time) simply return 0
 * immediately.
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
/* Current solution is not very complete.
 * Iconv might well be available when a usable nl_langinfo is not.
 * In this case we it should be possible to determine charset through other means
 * See http://www.cl.cam.ac.uk/~mgk25/unicode.html#activate for more info on a possible solution */
 
#if defined(HAVE_ICONV_H) && defined(HAVE_NL_LANGINFO) && defined(CODESET)
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
	/* On some systems and with libiconv, arg1 is const char **. 
	 * ICONV_CONST is defined by configure if the system needs this */
	if (iconv(converter,
			(ICONV_CONST char **)&readCursor,inputOutputByteSize,
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
#endif
	return 0;
}



/**
 * Encodes a buffer of data onto hexadecimal ASCII.
 *
 * The resulting data is made up of pairs of ASCII hex-digits,
 * space- and newline-separated. This is the counterpart of
 * unencode().
 *
 * The @c returned must allocated prior you call this function and won't
 * be bigger than 3 times the size of the source @c unencoded.
 * 
 *
 * @param unencoded the source buffer.
 * @param size the size of the source buffer in bytes.
 * @param returned the preallocated destination for the ASCII-encoded data.
 * @return the amount of bytes used in the resulting encoded buffer.
 * @see unencode()
 * @ingroup backend
 */
ssize_t encode(void *unencoded, size_t size, char *returned) {
	char *readCursor=unencoded;
	char *writeCursor=returned;
	int blockStep=4; /* 4 bytes per block */
	int lineStep=8*blockStep; /* 8 blocks per line */
	int currentInBlock=0;
	int currentInLine=0;

	while ((readCursor-(char *)unencoded)<size) {
		sprintf(writeCursor,"%02x",*(unsigned char *)readCursor);
		readCursor++;
		writeCursor+=2;
		currentInBlock++;
		currentInLine++;
		if (currentInLine==lineStep) {
			*writeCursor='\n'; writeCursor++;
			currentInLine=0;
			currentInBlock=0;
		}
		if (currentInBlock==blockStep) {
			*writeCursor=' '; writeCursor++;
			currentInBlock=0;
		}
	}
	*writeCursor='\n';
	*++writeCursor=0;
	return writeCursor-returned;
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
 * @return 0 on success, other value otherwise, and @c errno is set
 * @see kdbGetValue(), keySetString(), kdbSetKey()
 * @ingroup kdb
 */
int kdbSetValue(KDBHandle handle, const char *keyname, const char *value) {
	Key *key;
	int rc;

/* TODO: check key type first */
	key=keyNew(keyname,KEY_SWITCH_END);
	rc=kdbGetKey(handle,key);
	keySetString(key,value);
	rc=kdbSetKey(handle,key);
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
	kdbGetValueByParent(handle,parent,keys[c],buffer,sizeof(buffer));
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
 * @Return whatever is returned by kdbSetValue()
 * @ingroup kdb
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
 * @return 0 on success, or what kdbGetKey() returns, and @c errno is set
 * @see kdbGetKey(), kdbGetValueByParent(), kdbGetKeyByParentKey()
 * @ingroup kdb
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
 * @ingroup kdb
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
 * This method is similar and calls kdbGetKeyChildKeys(). It is provided for
 * convenience.
 * @ingroup kdb
 */
ssize_t kdbGetChildKeys(KDBHandle handle, const char *parentName, KeySet *returned, unsigned long options) {
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
 * @param returned the initialized KeySet to be filled
 * @return the number of root keys found
 * @see #KeyNamespace
 * @see commandList() code in kdb command for usage example
 * @ingroup kdb
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
 * @param key a pointer to a Key that has a name set
 * @return 0 on success, or other value and @c errno is set
 * @see kdbSetKey()
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
 * A high level, probably inefficient implementation for the kdbSetKeys()
 * method. If a backend doesn't want to reimplement this method, this
 * implementation can be used, in which kdbSetKey() will be called for
 * each Key object contained in @p ks.
 *
 * @see kdbSetKeys(), kdbSetKeys_backend()
 *
 * @ingroup backend
 */
int kdbSetKeys_default(KDBHandle handle, KeySet *ks) {
	Key *current=ksCurrent(ks);
	int ret;

	if (!current) current=ksNext(ks);
	while (current) {
		if (keyNeedsSync(current))
			if ((ret=kdbSetKey(handle,current))) /* check error */
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
 * @param key the key to be renamed
 * @param newName the new key name
 * @return 0 on success, or whathever is returned by the backend
 * 	implementation on failure, and @c errno is propagated
 * @ingroup kdb
 */
int kdbRename(KDBHandle handle, Key *key, const char *newName) {
	int rc=0;
	
	if (handle && handle->kdbRename)
		rc=handle->kdbRename(handle,key,newName);
	else {
		errno=KDB_RET_NOSYS;
		return -1;
	}
	
	return rc;
}



/**
 * Remove a key from the backend storage.
 * The @c key object will not be freed. It is your responsability
 * to keyDel() it after kdbRemoveKey().
 *
 * This method is not recursive.
 *
 * @param key the key to be removed
 * @return 0 on success, or whathever is returned by the backend
 * 	implementation on failure, and @c errno is propagated
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
 * Remove a key by its name from the backend storage.
 * This is a convenience to kdbRemoveKey().
 *
 * @param keyName the name of the key to be removed
 * @return 0 on success, or whathever is returned by kdbRemoveKey(),
 * 	and @c errno is propagated
 * @see commandRemove() code in kdb command for usage example
 * @ingroup kdb
 */
int kdbRemove(KDBHandle handle, const char *keyName) {
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
 * A high level, probably inefficient, implementation for the kdbMonitorKey()
 * method. If a backend doesn't want to reimplement this method, this
 * implementation can be used.
 *
 * @ingroup backend
 */
uint32_t kdbMonitorKey_default(KDBHandle handle, Key *interest,
		uint32_t diffMask, unsigned long iterations, unsigned sleeptime) {
	Key *tested;
	int rc;
	uint32_t diff;
	int infinitum=0;

	/* consistency */
	if (!interest || !keyGetNameSize(interest)) return 0;

	/* Unacceptable 0 usecs sleep. Defaults to 1 second */
	if (!sleeptime) sleeptime=1000;

	if (!iterations) infinitum=1;
	else infinitum=0;

	/* Work with a copy of the key */
	tested=keyNew(0);
	keyDup(interest,tested);

	while (infinitum || --iterations) {
		rc=kdbGetKey(handle,tested);
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
		if (infinitum || iterations) usleep(sleeptime);
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
int kdbhSetBackendData(KDBHandle handle, void *data) {
	handle->backendData = data;
	return 0;
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
void *kdbhGetBackendData(KDBHandle handle) {
	return handle->backendData;
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









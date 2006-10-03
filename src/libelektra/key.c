/***************************************************************************
                          key.c  -  Methods for Key manipulation
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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef WIN32
#include <io.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#ifdef HAVE_GRP_H
#include <grp.h>
#endif

#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif

#include "kdb.h"
#include "kdbprivate.h"

/**
 * Calculates the lenght in bytes of a string.
 * 
 * This function differs from strlen() because it is Unicode and multibyte
 * chars safe. While strlen() counts characters and ignores the final NULL,
 * strblen() count bytes including the ending NULL.
 * @return number of bytes used by the string, including the final NULL.
 * @ingroup backend
 */
size_t strblen(const char *s) {
	char *found=strchr(s,0);
	if (found) return found-s+1;
	return 0;
}



/**
 * @defgroup keybase Key :: Basic Methods
 * @brief Key construction and initialization methods.
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * A Key is the essential class that encapsulates key @link keyname name @endlink,
 * @link keyvalue value @endlink and @link keymeta metainfo @endlink.
 * Key properties are:
 * - @link keyname Key name @endlink
 * - @link keyvalue Key value @endlink
 * - @link keySetType() Data type @endlink
 * - @link keyGetComment() Key comment @endlink
 * - @link keyGetOwner() User domain @endlink (the user that owns the key)
 * - @link keymeta UID, GID and filesystem-like access permissions @endlink
 * - @link keymeta Access, change and modification times @endlink
 * - @link keySetFlag() A general flag @endlink
 *
 * Described here the methods to allocate and free the key.
 *
 */

 
 
/**
 * @defgroup keyname Key :: Name Manipulation Methods
 * @brief Methods to do various operations on Key names.
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 *
 * @par Rules for Key Names
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
 *
 *
 */


/**
 * @defgroup keyvalue Key :: Value Manipulation Methods
 * @brief Methods to do various operations on Key values.
 *
 * A key can contain a value in different format. The most
 * likely situation is, that the value is interpreted as
 * text. Use keyGetString() for that.
 * You can save any Unicode Symbols and Elektra will
 * take care that you get the same back, independent of
 * your current environment.
 *
 * In some situations this idea fails. When you need exactly
 * the same value back without any interpretation of the
 * characters, there is keySetBinary(). If you use that, its
 * very likely that your Configuration is not according
 * to the standard. Also for Numbers, Booleans and Date you
 * should use keyGetString(). To do so, you might use strtod()
 * strtol() and then atol() or atof() to convert back.
 * 
 * A key may also be just a link. Here you will also find
 * the manipulation methods for keyGetLink().
 * 
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 *
 */


/**
 * @defgroup keymeta Key :: Meta Info Manipulation Methods
 * @brief Methods to do various operations on Key metainfo
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * Key metainfo are:
 * - Comment about the key
 * - User domain
 * - UID, GID and filesystem-like access permissions
 * - Access, change and modification times
 * - A general flag
 *
 * The comment can contain userdata which directly
 * belong to that key.
 *
 * User domain is the user that owns the key. It only
 * works for the user/ hierachy.
 * 
 * Every user and group of your System has a uniqe ID.
 * These values are used in the keys too. They are
 * very important for the access. See man 2 chown.
 *
 * With the access mode you can choose if a user, group
 * or the world can access your key. See man 2 chmod.
 *
 */


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


/**
 * @defgroup keymisc Key :: Miscelaneous
 * @brief Methods to do various things
 *
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 *
 */




/**
 * A practical way to fully create a Key object in one step.
 * This function tries to mimic the C++ way for constructors.
 *
 * Due to ABI compatibility, the @p Key structure is not defined in kdb.h,
 * only declared. So you can only declare @p pointers to @p Keys in your
 * program, and allocate and free memory for them with keyNew()
 * and keyDel() respectively.
 * See http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html#AEN135
 * 
 * You can call it in many different ways depending on the attribute tags you
 * pass as parameters. Tags are represented as the #KeySwitch values,
 * and tell keyNew() which Key attribute comes next.
 * 
 * The simplest and minimum way to use it is with no tags, only a key name:
 * @code
Key *nullKey,*emptyNamedKey;

// Create a key that has no name, is completely empty, but is initialized
nullKey=keyNew(KEY_SWITCH_END);

// Create and initialize a key with a name and nothing else
emptyNamedKey=keyNew("user/some/example",KEY_SWITCH_END);
 * @endcode
 *
 * keyNew() allocates memory for a key object and then calls keyInit(). After
 * that, it processes the given argument list.
 * 
 * The Key attribute tags are the following:
 * - KeySwitch::KEY_SWITCH_TYPE \n
 *   Next parameter is a type of the value from #KeyType or a custom type.
 *   You must use this tag before KeySwitch::KEY_SWITCH_VALUE, otherwise
 *   KeyType::KEY_TYPE_STRING is assumed.
 * - KeySwitch::KEY_SWITCH_VALUE \n
 *   Next parameter is a pointer to the value that will be set to the key
 *   If no KeySwitch::KEY_SWITCH_TYPE was used before,
 *   KeySwitch::KEY_TYPE_STRING is assumed. If KEY_SWITCH_TYPE was previously
 *   passed with a KEY_TYPE_BINARY or any other custom binary type (see
 *   keySetType()) as parameter, one parameter is needed to define the binary
 *   value size. See the example bellow.
 * - KeySwitch::KEY_SWITCH_UID, @p KeySwitch::KEY_SWITCH_GID \n
 *   Next parameter is taken as the UID (uid_t) or GID (gid_t) that will
 *   be defined on the key. See keySetUID() and keySetGID().
 * - KeySwitch::KEY_SWITCH_MODE \n
 *   Next parameter is taken as access permissions (mode_t) to the key.
 *   See keySetAccess().
 * - KeySwitch::KEY_SWITCH_UMODE \n
 *   Next parameter is taken as user's umask, and will be used to calculate
 *   and set key's access permissions. If used after KEY_TYPE_DIR
 *   with KEY_SWITCH_TYPE, keySetDir() will be used. See keySetAccess(). Do
 *   not use this switch with KEY_SWITCH_MODE.
 * - KeySwitch::KEY_SWITCH_DOMAIN \n
 *   Next parameter is the user domain. See keySetOwner().
 * - KeySwitch::KEY_SWITCH_COMMENT \n
 *   Next parameter is a comment. See keySetComment().
 * - KeySwitch::KEY_SWITCH_NEEDSYNC \n
 *   Next parameter is a KDBHandle. Makes keyNew() retrieve the Key from the
 *   backend with kdbGetKey(). In the same keyNew() call you can use this
 *   tag in conjunction with any other, which will make keyNew() modify
 *   only some attributes after retrieving the key, and return it to you.
 *   Order of parameters do matter. If the internal call to kdbGetKey()
 *   failed, you'll still have a valid, but flaged, key.
 *   Check with keyGetFlag(), and @p errno. You will have to kdbOpen()
 *   before using keyNew() with this tag.
 * - KeySwitch::KEY_SWITCH_END \n
 *   Must be the last parameter passed to keyNew(). It is always
 *   required, unless the @p keyName is NULL too.
 *
 * @par Example:
 * @code
KeySet *ks=ksNew();

kdbOpen();
	
ksAppend(ks,keyNew(KEY_SWITCH_END));       // an empty key
	
ksAppend(ks,keyNew("user/sw",              // a simple key
	KEY_SWITCH_END));                      // no more args
	
ksAppend(ks,keyNew("system/sw",
	KEY_SWITCH_NEEDSYNC, handle,           // a key retrieved from storage
	KEY_SWITCH_END));                      // end of args
	
ksAppend(ks,keyNew("user/tmp/ex1",
	KEY_SWITCH_VALUE,"some data",          // with a simple value
	KEY_SWITCH_END));                      // end of args
	
ksAppend(ks,keyNew("user/tmp/ex2",
	KEY_SWITCH_VALUE,"some data",          // with a simple value
	KEY_SWITCH_MODE,0777,                  // permissions
	KEY_SWITCH_END));                      // end of args
	
ksAppend(ks,keyNew("user/tmp/ex3",
	KEY_SWITCH_TYPE,KEY_TYPE_LINK,         // only type
	KEY_SWITCH_VALUE,"system/mtp/x",       // link destination
	KEY_SWITCH_MODE,0654,                  // weird permissions
	KEY_SWITCH_END));                      // end of args
	
ksAppend(ks,keyNew("user/tmp/ex4",
	KEY_SWITCH_TYPE,KEY_TYPE_BINARY,       // key type
	KEY_SWITCH_VALUE,"some data",7,        // value that will be truncated in 7 bytes
	KEY_SWITCH_COMMENT,"value is truncated",
	KEY_SWITCH_DOMAIN,"root",              // owner (not uid) is root
	KEY_SWITCH_UID,0,                      // root uid
	KEY_SWITCH_END));                      // end of args

ksAppend(ks,keyNew("user/tmp/ex5",
	KEY_SWITCH_TYPE,KEY_TYPE_DIR,          // dir key with...
	KEY_SWITCH_TYPE,KEY_TYPE_BINARY,       // ...a binary value
	KEY_SWITCH_VALUE,"some data",7,        // value that will be truncated in 7 bytes
	KEY_SWITCH_COMMENT,"value is truncated",
	KEY_SWITCH_DOMAIN,"root",              // owner (not uid) is root
	KEY_SWITCH_UID,0,                      // root uid
	KEY_SWITCH_END));                      // end of args

ksAppend(ks,keyNew("user/env/alias/ls",    // a key we know we have
	KEY_SWITCH_NEEDSYNC, handle,           // retrieve from storage, passing the KDB handle
	KEY_SWITCH_END));                      // do nothing more
	
ksAppend(ks,keyNew("user/env/alias/ls",    // same key
	KEY_SWITCH_NEEDSYNC, handle,           // retrieve from storage
	KEY_SWITCH_DOMAIN,"root",              // set new owner (not uid) as root
	KEY_SWITCH_COMMENT,"new comment",      // set new comment
	KEY_SWITCH_END));                      // end of args
	
ksToStream(ks,stdout,KDB_O_XMLHEADERS);
	
ksDel(ks);
kdbClose();
 * @endcode
 *
 * @param keyName a valid name to the key, or NULL to get a simple
 * 	initialized, but really empty, object 
 * @see keyDel()
 * @return a pointer to a new allocated and initialized Key object,
 * 	or NULL if an invalid @p keyName was passed (see keySetName()).
 * @ingroup keybase
 * 
 */
Key *keyNew(const char *keyName, ...) {
	va_list va;
	Key *key;
	uint32_t action=0;
	uint8_t keyType=KEY_TYPE_UNDEFINED;
	uint8_t keyTypeBinary=0; /* a boolean shortcut */
	size_t valueSize=0;
	
	key=(Key *)malloc(sizeof(Key));
	if (!key) return 0;
	keyInit(key);
	
	if (keyName) {
		size_t nameSize;
		
		nameSize=keySetName(key,keyName);
		if (! nameSize) {
			free(key);
			return 0;
		}
		
		va_start(va,keyName);
		
		action=va_arg(va,uint32_t);
		while (action) {
			switch (action) {
				case KEY_SWITCH_TYPE:
					/* We are waiting for 1 or 2 parameters
					 * following this action */
					
					/* First is the type */
					keyType=(uint8_t)va_arg(va,unsigned int);
					
					if (keyType == KEY_TYPE_DIR) {
						mode_t mask=umask(0);
						
						umask(mask);
						keySetDir(key,mask);
						keyType=KEY_TYPE_UNDEFINED;
					} else {
						keyTypeBinary=(KEY_TYPE_BINARY <= keyType &&
							keyType < KEY_TYPE_STRING);
					
						keySetType(key,keyType);
					}
					break;
				case KEY_SWITCH_VALUE:
					if (keyType == KEY_TYPE_UNDEFINED)
						keyType=KEY_TYPE_STRING;

					if (!keyTypeBinary) {
						/* most popular cases */
						keySetString(key,va_arg(va,char *));
						/* reset the type due to the
						 * above keySetString override */
						keySetType(key,keyType);
					} else {
						/* Binary val: we need first the value then the size */
						void *value=va_arg(va,void *);
						valueSize=va_arg(va,size_t);
						keySetRaw(key,value,valueSize);
					}

					break;
				case KEY_SWITCH_UID:
					keySetUID(key,va_arg(va,uid_t));
					break;
				case KEY_SWITCH_GID:
					keySetGID(key,va_arg(va,gid_t));
					break;
				case KEY_SWITCH_MODE:
					keySetAccess(key,va_arg(va,mode_t));
					break;
				case KEY_SWITCH_UMODE:
					if (keyIsDir(key))
						keySetDir(key,va_arg(va,mode_t));
					else
						keySetUAccess(key,va_arg(va,mode_t));
					break;
				case KEY_SWITCH_DOMAIN:
					keySetOwner(key,va_arg(va,char *));
					break;
				case KEY_SWITCH_COMMENT:
					keySetComment(key,va_arg(va,char *));
					break;
				case KEY_SWITCH_NEEDSYNC: {
					int rc=0;
					rc=kdbGetKey(va_arg(va,KDBHandle),key);
					if (rc)
						/* Flag the key to indicate an error in
						 * kdbGetKey(). Propagated errno will indicate
						 * the error ocurred. */
						key->flags|=KEY_SWITCH_FLAG; /* same as keySetFlag(key) */
				} break;
			}
			action=va_arg(va,uint32_t);
		}
		va_end(va);
	}
	return key;
}
 
 
 
 
/**
 * A destructor for Key objects. Every key created by keyNew() must be
 * deleted with keyDel().
 * It will keyClose() and free() the @p key pointer.
 *
 * There is the @p keyFree() macro if you prefer this method name.
 * 
 * @see keyNew()
 * @return whatever is returned by keyClose()
 * @ingroup keybase
 *
 */ 
int keyDel(Key *key) {
	int rc;
	
	rc=keyClose(key);
	free(key);
	
	return rc;
 }

 



/**
 * Test if @p key is initialized.
 *
 * This function is more or less reliable.
 * You'd better guarantee your code is robust enough using
 * keyNew(), keyInit(), keyDel() and keyClose() everytime.
 * 
 * @ingroup keytest
 * 
 */
int keyIsInitialized(const Key *key) {
	/* if (!key) return 0; */ /* removing to activate stupid mode */
	return ((key->flags & KEY_SWITCH_INITMASK)==KEY_SWITCH_INITIALIZED);
}


/*******************************************
 *    General name manipulation methods    *
 *******************************************/


/**
 * Returns one level of the key name.
 *
 * This method is used to skip repeating '/' and to find escaping chars.
 * Given @p keyName, this method returns a pointer to the next name level
 * found and changes @p size to the number of bytes on this level name.
 *
 * This method is used by keySetName() and others to cleanup parameters
 * before being accepted in the Key object.
 *
 * @code
// Lets define a key name with a lot of repeating '/' and escaped '/'
char *keyName="user////abc/def\/ghi////jkl///";
char *p;
size_t size=0;
int level=0;
char buffer[20];

p=keyName;
while (*(p=keyNameGetOneLevel(p+size,&size)!=0) {
	level++;

	// copy what we found to a buffer, so we can NULL-terminate it
	strncpy(buffer,p,size);
	buffer[size]=0;

	printf("Level %d name: \"%s\"\n",level,buffer);
}
 * @endcode
 *
 * The above example will produce the following output:
 *
 * @code
Level 1 name: user
Level 2 name: abc
Level 3 name: def\/ghi
Level 4 name: jkl
 * @endcode
 *
 * @param keyName the string that will be searched
 * @param size the number of bytes of level name found in @p keyName until
 * 	the next delimiter ('/')
 * @return a pointer to the first char of the next level name
 * @ingroup keyname
 */
char *keyNameGetOneLevel(const char *keyName, size_t *size) {
	char *real=(char *)keyName;
	size_t cursor=0;
	int escapeNext=0;
	int end=0;
	
	/* skip all repeating '/' in the begining */
	while (*real && *real == RG_KEY_DELIM) real++;
	
	/* now see where this basename ends handling escaped chars with '\' */
	while (real[cursor] && ! end) {
		switch (real[cursor]) {
			case '\\':
				escapeNext=1;
				break;
			case RG_KEY_DELIM:
				if (! escapeNext) end=1;
			default:
				escapeNext=0;
		}
		++cursor;
	}
	
	/* if a '/' stopped our loop, balance the counter */
	if (end) --cursor;
	
	*size=cursor;
	return real;
}


/**
 * Set a new name to a key.
 *
 * A valid name is of the forms:
 * - @p system/something
 * - @p user/something
 * - @p user:username/something
 *
 * The last form has explicitly set the user domain, to let the library
 * know in which user folder to save the key. A user domain is a user name.
 * If not defined (the second form) current user is calculated and used
 * as default.
 *
 * You should always follow the guidelines for key tree structure creation at
 * @ref rules.
 *
 * A private copy of the key name will be stored, and the @p newName
 * parameter can be freed after this call.
 *
 * @return size in bytes of this new key name including ending NULL, or 0
 * 	if @p newName is empty, or if @p newName is invalid, in which
 * 	case @c errno is set to KDBErrr::KDB_RET_INVALIDKEY.
 * @param key the key object
 * @param newName the new key name
 * @see keyNew(), keySetOwner()
 * @see keyGetName(), keyGetFullName(), keyStealName()
 * @ingroup keyname
 */

ssize_t keySetName(Key *key, const char *newName) {
	size_t length;
	size_t rootLength, userLength, systemLength, userDomainLength;
	size_t keyNameSize=1; /* equal to length plus room for \0 */
	char *p=0;
	size_t size=0;
	
	/* handle null new key name, removing the old key */
	if (!newName || !(length=strblen(newName)-1)) {
		if (key->key) {
			free(key->key);
			key->key=0;
		}
		key->flags &= ~(KEY_SWITCH_NAME | KEY_SWITCH_NEEDSYNC |
				KEY_SWITCH_ISSYSTEM | KEY_SWITCH_ISUSER);
		return 0;
	}

	rootLength=keyNameGetFullRootNameSize(newName)-1;
	if (!rootLength) {
		errno=KDB_RET_INVALIDKEY;
		return -1;
	}
	userLength=sizeof("user")-1;
	systemLength=sizeof("system")-1;
	userDomainLength=rootLength-userLength;
	
	if (userDomainLength>0) --userDomainLength;

	if (keyNameIsUser(newName)) {
		/* handle "user*" */
		if (length > userLength) {
			/* handle "user?*" */
			if (*(newName+userLength)==':') {
				/* handle "user:*" */
				if (userDomainLength > 0) {
					p=realloc(key->userDomain,userDomainLength+1);
					if (NULL==p) goto error_mem;
					key->userDomain=p;
					strncpy(key->userDomain,newName+userLength+1,userDomainLength);
					key->userDomain[userDomainLength]=0; 
					key->flags |= KEY_SWITCH_DOMAIN;
				}
				keyNameSize+=length-userDomainLength-1;  /* -1 is for the ':' */
			} else if (*(newName+userLength)!=RG_KEY_DELIM) {
				/* handle when != "user/ *" */
				errno=KDB_RET_INVALIDKEY;
				return -1;
			} else {
				/* handle regular "user/ *" */
				keyNameSize+=length;
			}
		} else {
			/* handle "user" */
			keyNameSize+=userLength;
		}
		
		if (!key->userDomain) {
			char *envVar;

			envVar = getenv("USER");
			if ( envVar ) {
				keySetOwner(key, envVar);
			} else {
			       	/* TODO: handle "can't find $USER envar" */
				keySetOwner(key, NULL);
		       	}
		}

		rootLength  = userLength;
		key->flags |= KEY_SWITCH_ISUSER;
		key->flags &= ~KEY_SWITCH_ISSYSTEM;
	} else if (keyNameIsSystem(newName)) {
		/* handle "system*" */
		if (length > systemLength && *(newName+systemLength)!=RG_KEY_DELIM) {
			/* handle when != "system/ *" */
			errno=KDB_RET_INVALIDKEY;
			return -1;
		}
		keyNameSize+=length;
		
		rootLength  = systemLength;
		key->flags |= KEY_SWITCH_ISSYSTEM;
		key->flags &= ~KEY_SWITCH_ISUSER;
	} else {
		/* Given newName is neither "system" or "user" */
		errno=KDB_RET_INVALIDKEY;
		return -1;
	}
	
	/*
	   At this point:
	   - key->key has no memory (re)allocated yet
	   - keyNameSize has number of bytes that will be allocated for key name
	     with already removed user domain (in case of newName contained
	     a domain)
	   - key->userDomain is already set if newName is "user*"
	   - rootLength is sizeof("user")-1 or sizeof("system")-1
	*/

	/* Allocate memory for key->key */
	p=malloc(keyNameSize);
	if (NULL==p) goto error_mem;
	if (key->key) free(key->key);
	key->key=p;

	/* here key->key must have a correct size allocated buffer */
	if (!key->key) return -1;

	/* copy the root of newName to final destiny */
	strncpy(key->key,newName,rootLength);
	
	/* skip the root */
	p=(char *)newName;
	size=0;
	p=keyNameGetOneLevel(p+size,&size);
	
	/* iterate over each single folder name removing repeated '/' and escaping when needed */
	keyNameSize=rootLength;
	while (*(p=keyNameGetOneLevel(p+size,&size))) {
		/* Add a '/' to the end of key name */
		key->key[keyNameSize]=RG_KEY_DELIM;
		keyNameSize++;
		
		/* carefully append basenames */
		memcpy(key->key+keyNameSize,p,size);
		keyNameSize+=size;
	}
	key->key[keyNameSize]=0; /* finalize string */

	key->flags |= KEY_SWITCH_NAME | KEY_SWITCH_NEEDSYNC;
	
	return keyNameSize+1;

	error_mem:
		errno=KDB_RET_NOMEM;
		return -1;
}




/**
 * Adds @p baseName to the current key name.
 *
 * Assumes that @p key is a directory. @p baseName is appended to it.
 * The function adds @c '/' if needed while concatenating.
 *
 * So if @p key has name @c "system/dir1/dir2" and this method is called with
 * @p baseName @c "mykey", the resulting key will have name
 * @c "system/dir1/dir2/mykey".
 *
 * @return the size in bytes of the new key name including the ending NULL
 * @see keySetBaseName()
 * @ingroup keyname
 *
 */
ssize_t keyAddBaseName(Key *key,const char *baseName) {
	size_t nameSize=0;
	size_t newSize=0;
	size_t size=0;
	char *p=0;

	if (key->key) nameSize=strblen(key->key);
	if (baseName) newSize=strblen(baseName);
	else return nameSize;
	
	/* At this point, newSize has size of the baseName string, +1 for NULL */
	
	if (newSize <= 1)
		/* baseName is empty or NULL, so we are done */
		return nameSize;
	
	if (key->key) {
		p=realloc(key->key,newSize+nameSize);
		if (NULL == p) {
			errno=KDB_RET_NOMEM;
			return -1;
		}
		
		/*strcpy(p,key->key);
		free(key->key); key->key=0;*/
		key->key=p;
		
		/* Start appending basenames */
		p=(char *)baseName;
		nameSize--;
		while (*(p=keyNameGetOneLevel(p+size,&size))) {
			/* Add a '/' to the end of key name */
			key->key[nameSize]=RG_KEY_DELIM;
			nameSize++;
		
			/* carefully append basenames */
			memcpy(key->key+nameSize,p,size);
			nameSize+=size;
		}
		key->key[nameSize]=0; /* finalize string */
		return nameSize+1;
	} else return keySetName(key,baseName);
}




/**
 * Sets @c baseName as the new basename for @c key.
 *
 * All text after the last @c '/' in the @p key keyname is erased and
 * @p baseName is appended.
 *
 * So lets suppose @p key has name @c "system/dir1/dir2/key1". If @p baseName
 * is @c "key2", the resulting key name will be @c "system/dir1/dir2/key2".
 * If @p baseName is empty or NULL, the resulting key name will
 * be @c "system/dir1/dir2".
 *
 * @return the size in bytes of the new key name or -1 on error, and
 *    @c errno is set to KDBErr::KDB_RET_NOMEM
 * @see keyAddBaseName()
 * @ingroup keyname
 */
ssize_t keySetBaseName(Key *key, const char *baseName) {
	size_t newSize=0;
	size_t size=0;
	char *p=0;
	char *prevParent=0;
	size_t parentSize=0;
	size_t oldSize=0;

	if (baseName) newSize=strblen(baseName);
	
	/* Consume entire parent name */
	p=key->key;
	while (*(p=keyNameGetOneLevel(p+size,&size)))
		prevParent=p;

	if (prevParent && prevParent!=key->key) {
		/* For a key like "abc/def/ghi/jkl" prevParent is now pointing
		   to the 'j'. Move it to previous char, which is probably '/'. */
		--prevParent;
	
		oldSize=strblen(key->key);
		parentSize = prevParent-key->key;
	
		/* For a key like "abc/def/ghi/jkl" parentSize now has size 11 which
		   is number of bytes from 'a' to 'i' */
	
		/* Strip old "/basename" */
		key->key[parentSize]=0;
	
		/* baseName is empty or NULL, so we are done */
		if (newSize <= 1) return parentSize+1;
		
		/* Check if we need to extend memory allocated for key->key */
		if (oldSize < (newSize += parentSize + 1)) {
			p=realloc(key->key,newSize);
			if (NULL == p) {
				errno=KDB_RET_NOMEM;
				return -1;
			}
		
			key->key=p;
		}
		
		/* Start appending basenames */
		size=0;
		p=(char *)baseName;
		while (*(p=keyNameGetOneLevel(p+size,&size))) {
			/* Add a '/' to the end of key name */
			key->key[parentSize]=RG_KEY_DELIM;
			parentSize++;
		
			/* carefully append basenames */
			memcpy(key->key+parentSize,p,size);
			parentSize+=size;
		}
		key->key[parentSize]=0; /* finalize string */
		return parentSize+1;
	} else return keySetName(key,baseName);
}




/**
 * Bytes needed to store the key name without user domain.
 *
 * @return number of bytes needed, including ending NULL, to store key name
 * 	without user domain
 * @see keyGetName(), keyGetFullNameSize()
 * @ingroup keyname
 */
ssize_t keyGetNameSize(const Key *key) {
	if (key->key) return strblen(key->key);
	else return 0;
}




/**
 * Get abreviated key name (without user domain name).
 *
 * @return number of bytes written to @p returnedName
 * @param key the key object
 * @param returnedName pre-allocated memory to write the key name
 * @param maxSize maximum number of bytes that will fit in returnedName, including the final NULL
 * @see keyGetNameSize(), keyGetFullName(), keyGetFullNameSize()
 * @ingroup keyname
 */
ssize_t keyGetName(const Key *key, char *returnedName, size_t maxSize) {
	size_t bytes;

	if (!key->key) {
		errno=KDB_RET_NOKEY;
		returnedName[0]=0;
		return 0;
	}

	bytes=strblen(strncpy(returnedName,key->key,maxSize));
	if (maxSize < strblen(key->key)) {
		errno=KDB_RET_TRUNC;
		return -1;
	}
	return bytes;
}



/**
 * Returns a pointer to the real internal @p key abreviated name (without
 * user domain name).
 * This is a much more efficient version of keyGetName() and you should use
 * it if you are responsible enough to not mess up things.
 *
 * @param key the key object
 * @see keyGetNameSize(), keyGetFullName(), keyGetFullNameSize()
 * @see keyStealValue() for an example
 * @ingroup keyname
 */
char *keyStealName(const Key *key) {
	return key->key;
}






/**
 * Bytes needed to store the key name including user domain and ending NULL.
 *
 * @return number of bytes needed to store key name including user domain
 * @see keyGetFullName(), keyGetNameSize()
 * @ingroup keyname
 */
ssize_t keyGetFullNameSize(const Key *key) {
	size_t returnedSize;

	if (!key->key) return 0;

	returnedSize=strblen(key->key);

	if (keyNameIsUser(key->key) && key->userDomain)
		returnedSize+=strblen(key->userDomain);

	/*
	   After 2 strblen() calls looks like we counted one more NULL.
	   But we need this byte count because a full key name has an
	   additional ':' char.
	*/
	
	return returnedSize;
}




/**
 * Get key full name, including the user domain name.
 *
 * @return number of bytes written
 * @param key the key object
 * @param returnedName pre-allocated memory to write the key name
 * @param maxSize maximum number of bytes that will fit in returnedName, including the final NULL
 * @ingroup keyname
 */
ssize_t keyGetFullName(const Key *key, char *returnedName, size_t maxSize) {
	size_t userSize=sizeof("user")-1;
	size_t userDomainSize;
	ssize_t length;
	char *cursor;

	length=keyGetFullNameSize(key);
	if (length == 0) {
		errno=KDB_RET_NOKEY;
		returnedName[0]=0;
		return length;
	}
	if (length < 0) return length;
	if (length > maxSize) {
		errno=KDB_RET_TRUNC;
		return -1;
	}

	cursor=returnedName;
	if (keyIsUser(key)) {
		strncpy(cursor,key->key,userSize);
		cursor+=userSize;
		if (key->userDomain) {
			*cursor=':'; ++cursor;
			userDomainSize=strblen(key->userDomain)-1;
			strcpy(cursor,key->userDomain);
			cursor+=userDomainSize;
		}
		strcpy(cursor,key->key+userSize);
	} else strcpy(cursor,key->key);

	return length;
}





/**
 * Return the namespace of a key name.
 *
 * Currently valid namespaces are KeyNamespace::KEY_NS_SYSTEM and KeyNamespace::KEY_NS_USER.
 *
 * @return KeyNamespace::KEY_NS_SYSTEM, KeyNamespace::KEY_NS_USER or 0
 * @see keyGetNamespace(), keyIsUser(), keyIsSystem()
 * @see #KeyNamespace
 * @ingroup keytest
 *
 */
int keyNameGetNamespace(const char *keyName) {
	if (keyNameIsSystem(keyName)) return KEY_NS_SYSTEM;
	if (keyNameIsUser(keyName)) return KEY_NS_USER;
	return 0;
}



/**
 * Return the namespace of a key
 *
 * Currently valid namespaces are KeyNamespace::KEY_NS_SYSTEM and KeyNamespace::KEY_NS_USER.
 *
 * @return KeyNamespace::KEY_NS_SYSTEM, KeyNamespace::KEY_NS_USER or 0
 * @see keyNameGetNamespace(), keyIsUser(), keyIsSystem()
 * @ingroup keytest
 *
 */
int keyGetNamespace(const Key *key) {
	if (key->flags & KEY_SWITCH_ISSYSTEM) return KEY_NS_SYSTEM;
	if (key->flags & KEY_SWITCH_ISUSER) return KEY_NS_USER;
	return 0;
}



/**
 * Check whether a key name is under the @p system namespace or not
 *
 * @return 1 if string begins with @p system , 0 otherwise
 * @param keyName the name of a key
 * @see keyIsSystem(), keyIsUser(), keyNameIsUser()
 * @ingroup keyname
 *
 */
int keyNameIsSystem(const char *keyName) {
	if (!strncmp("system",keyName,sizeof("system")-1)) return 1;
	return 0;
}



/**
 * Check whether a key is under the @p system namespace or not
 *
 * @return 1 if key name begins with @p system, 0 otherwise
 * @see keyNameIsSystem(), keyIsUser(), keyNameIsUser()
 * @ingroup keytest
 *
 */
int keyIsSystem(const Key *key) {
	return (key->flags & KEY_SWITCH_ISSYSTEM)?1:0;
}



/**
 * Check whether a key name is under the @p user namespace or not
 *
 * @return 1 if string begins with @p user, 0 otherwise
 * @param keyName the name of a key
 * @see keyIsSystem(), keyIsUser(), keyNameIsSystem()
 * @ingroup keyname
 *
 */
int keyNameIsUser(const char *keyName) {
	if (!strncmp("user",keyName,sizeof("user")-1)) return 1;
	return 0;
}



/**
 * Check whether a key is under the @p user namespace or not
 *
 * @return 1 if key name begins with @p user, 0 otherwise
 * @see keyNameIsSystem(), keyIsSystem(), keyNameIsUser()
 * @ingroup keytest
 *
 */
int keyIsUser(const Key *key) {
	return (key->flags & KEY_SWITCH_ISUSER)?1:0;
}



/**
 * Gets number of bytes needed to store root name of a key name
 *
 * Possible root key names are @p system, @p user or @p "user:someuser" .
 *
 * @return number of bytes needed with ending NULL
 * @param keyName the name of the key
 * @see keyGetFullRootNameSize()
 * @ingroup keyname
 */
ssize_t keyNameGetFullRootNameSize(const char *keyName) {
	char *end;
	int length=strlen(keyName);

	if (!length) return 0;

	/*
		Possible situations:
		user:someuser
		user:someuser/
		user:someuser/key/name
		user:some.user/key/name
		.
		\.
		(empty)
	*/
	end=strchr(keyName,RG_KEY_DELIM);
	if (!end) /* Reached end of string. Root is entire key. */
		end = (char *)keyName + length;

	return end-keyName+1;
}



/**
 * Gets number of bytes needed to store root name of a key.
 *
 * Possible root key names are @p system or @p user .
 * This method does not consider the user domain in @p user:username keys.
 *
 * @return number of bytes needed with the ending NULL
 * @see keyGetFullRootNameSize(), keyNameGetRootNameSize()
 * @ingroup keyname
 */
ssize_t keyGetRootNameSize(const Key *key) {
	if (!key->key) return 0;

	if (keyIsUser(key)) return sizeof("user");
	else return sizeof("system");
}



/**
 * Copy to @p returned the root name of @p key.
 *
 * Some examples:
 * - root of @p system/some/key is @p system
 * - root of @p user:denise/some/key is @p user
 * - root of @p user/env/env1 is @p user
 *
 * Use keyGetFullRootName() to get also the user domain.
 *
 * @param key the key to extract root from
 * @param returned a pre-allocated buffer to store the rootname
 * @param maxSize size of the @p returned buffer
 * @return number of bytes needed with ending NULL
 * @see keyNameGetRootNameSize(), keyGetRootNameSize(), keyGetFullRootName()
 * @ingroup keyname
 */
ssize_t keyGetRootName(const Key *key, char *returned, size_t maxSize) {
	size_t size;

	if (!key->key) {
		errno=KDB_RET_NOKEY;
		return -1;
	}

	if (!(size=keyGetRootNameSize(key))) {
		errno=KDB_RET_NOKEY;
		return -1;
	}

	if (maxSize < size) {
		errno=KDB_RET_TRUNC;
		return -1;
	} else strncpy(returned,key->key,size-1);
	returned[size-1]=0; /* null terminate it */
	return size;
}






/**
 * Gets number of bytes needed to store root name of a key name without
 * user domain.
 *
 * Some examples:
 * - root of @p system/some/key is @p system
 * - root of @p user:denise/some/key is @p user
 * - root of @p user/env/env1 is @p user
 *
 * @param keyName the name of the key
 * @return number of bytes needed with ending NULL or -1 if @p keyName is
 * 	not a valid name and @c errno is set to KDBErr::KDB_RET_INVALIDKEY
 * @see keyGetRootNameSize()
 * @ingroup keyname
 */
ssize_t keyNameGetRootNameSize(const char *keyName) {
	int length=strlen(keyName);

	if (!length) return 0;

	/*
		Possible situations:
	user
	user/
	user/blabla
	user:someuser
	user:someuser/
	user:someuser/key/name
	system
	system/
	system/blabla
	(empty)
	*/
	
	if (keyNameIsUser(keyName)) return sizeof("user");
	else if (keyNameIsSystem(keyName)) return sizeof("system");
	else {
		errno=KDB_RET_INVALIDKEY;
		return -1;
	}
}





/**
 * Calculates number of bytes needed to store full root name of a key.
 *
 * Possible root key names are @p system, @p user or @p user:someuser.
 * In contrast to keyGetRootNameSize(), this method considers the user
 * domain part, and you should prefer this one.
 *
 * @return number of bytes needed with ending NULL
 * @see keyNameGetRootNameSize(), keyGetRootNameSize()
 * @ingroup keyname
 */
ssize_t keyGetFullRootNameSize(const Key *key) {
	size_t size=0;

	if (keyIsUser(key)) {
		if (key->userDomain) size=strblen(key->userDomain);
		else size=strblen(getenv("USER"));
		
		return size+sizeof("user");
	} else {
		return keyNameGetRootNameSize(key->key);
	}
}


/**
 * Copy to @p returned the full root name of the key.
 *
 * Some examples:
 * - root of @p system/some/key is @p system
 * - root of @p user:denise/some/key is @p user:denise
 * - root of @p user/env/env1 is @p user:$USER
 *
 * This method is more robust then keyGetRootName()
 *
 * @param key the key to extract root from
 * @param returned a pre-allocated buffer to store the rootname
 * @param maxSize size of the @p returned buffer
 * @return number of bytes written to @p returned including ending NULL
 * @see keyGetFullRootNameSize(), keyGetRootName()
 * @ingroup keyname
 */
ssize_t keyGetFullRootName(const Key *key, char *returned, size_t maxSize) {
	size_t size;
	size_t rootSize;
	char *cursor;

	if (!key->key) {
		errno=KDB_RET_NOKEY;
		return 0;
	}

	if (!(size=keyGetFullRootNameSize(key))) {
		errno=KDB_RET_NOKEY;
		return 0;
	}

	if (maxSize < size) {
		errno=KDB_RET_TRUNC;
		return -1;
	}
	
	rootSize = keyGetRootNameSize(key)-1;
	strncpy(returned,key->key, rootSize); /* copy "user" or "system" */
	if (keyIsUser(key)) {
		cursor = returned + rootSize;
		*cursor = ':'; cursor++;
		if (key->userDomain)
			strncpy (cursor, key->userDomain, size - rootSize);
		else
			strncpy (cursor, getenv("USER"),  size - rootSize);
	} else {
		returned[rootSize]=0;
	}

	return size;
}






/**
 * Get the number of bytes needed to store this key's parent name without
 * user domain, and with the ending NULL.
 * 
 * @see keyGetParentName() for example
 * @ingroup keyname
 */
ssize_t keyGetParentNameSize(const Key *key) {
	char *parentNameEnd=0;
	char *p;
	size_t size;

	if (!key->key) {
		errno=KDB_RET_NOKEY;
		return 0;
	}

	/*
		user   (size=0)
		user/parent/base       (size=sizeof("user/parent"))
		user/parent/base/      (size=sizeof("user/parent"))
		user/parent/base\/name (size=sizeof("user/parent"))
	*/

	/* initialize */
	p=key->key;
	size=0;
	
	/* iterate over level names */
	while (*(p=keyNameGetOneLevel(p+size,&size))) parentNameEnd=p;
	
	/* handle NULL or root key ("user" or "system") */
	if (!parentNameEnd || parentNameEnd==key->key) return 0;

	/* at this point, parentNameEnd points to the first char of the basename */
	/* example: it points to the 'b' of "user/key/basename" */
	
	/* the delta is the size we want */
	return parentNameEnd - key->key;
}



/**
 * Copy this key's parent name (without user domain) into a pre-allocated
 * buffer.
 *
 * @see keyGetParentNameSize()
 * @param returnedParent pre-allocated buffer to copy parent name to
 * @param maxSize number of bytes pre-allocated
 * @return number of bytes copied including ending NULL
 * @par Example:
 * @code
Key *key=keyNew("system/parent/base",KEY_SWITCH_END);
char *parentName;
size_t parentSize;

parentSize=keyGetParentNameSize(key);
parentName=malloc(parentSize);
keyGetParentName(key,parentName,parentSize);
 * @endcode
 * @ingroup keyname
 */
ssize_t keyGetParentName(const Key *key, char *returnedParent, size_t maxSize) {
	ssize_t parentSize;

	parentSize=keyGetParentNameSize(key);

	if (parentSize > maxSize) {
		errno=KDB_RET_TRUNC;
		return 0;
	} else strncpy(returnedParent,key->key,parentSize);

	returnedParent[parentSize-1]=0; /* ending NULL */
	
	return parentSize;
}






/**
 * Calculates number of bytes needed to store a basename of a key name
 * including the ending NULL.
 *
 * Key names that have only root names (e.g. @c "system" or @c "user"
 * or @c "user:domain" ) does not have basenames, thus the function will
 * return 0 bytes.
 *
 * Basenames are denoted as:
 * - @c system/some/thing/basename -> @c basename
 * - @c user:domain/some/thing/base\/name -> @c base\/name
 *
 * @return size in bytes of basename including ending NULL
 * @see keyGetBaseNameSize()
 * @ingroup keyname
 */
ssize_t keyNameGetBaseNameSize(const char *keyName) {
	char *p=(char *)keyName;
	char *base=0;
	size_t size=0;
	size_t baseSize=0;
	
	while (*(p=keyNameGetOneLevel(p+size,&size))) {
		base=p;
		baseSize=size;
	}
	
	if (base == keyName) return 0;
	else return baseSize+1;
}



/**
 * Calculates number of bytes needed to store basename of @p key.
 * Key names that have only root names (e.g. @c "system" or @c "user"
 * or @c "user:domain" ) does not have basenames, thus the function will
 * return 0 bytes.
 *
 * Basenames are denoted as:
 * - @c system/some/thing/basename -> @c basename
 * - @c user:domain/some/thing/base\/name > @c base\/name
 *
 * @return size in bytes of @p key's basename including ending NULL
 * @see keyNameGetBaseNameSize()
 * @ingroup keyname
 */
ssize_t keyGetBaseNameSize(const Key *key) {
	if (!key->key) return 0;

	return keyNameGetBaseNameSize(key->key);
}



/**
 * Calculate the basename of a key name and put it in @p returned finalizing
 * the string with NULL.
 *
 * Some examples:
 * - basename of @c system/some/keyname is @c keyname
 * - basename of @c "user/tmp/some key" is @c "some key"
 *
 * @param key the key to extract basename from
 * @param returned a pre-allocated buffer to store the basename
 * @param maxSize size of the @p returned buffer
 * @return number of bytes copied to @p returned, or 0 and @c errno is set
 * @see keyStealBaseName(), keyGetBaseNameSize()
 * @ingroup keyname
 */
ssize_t keyGetBaseName(const Key *key, char *returned, size_t maxSize) {
	size_t size=0;
	char *p=key->key;
	char *baseName=0;
	size_t baseSize=0;

	while (*(p=keyNameGetOneLevel(p+size,&size))) {
		baseName=p;
		baseSize=size+1;
	}
	
	if (!baseName || baseName==key->key) return 0;

	if (maxSize < baseSize) {
		strncpy(returned,baseName,maxSize);
		errno=KDB_RET_TRUNC;
		return maxSize;
	} else {
		strncpy(returned,baseName,baseSize);
		return baseSize;
	}
}


/**
 * Returns a pointer to the real internal key name where the basename starts.
 *
 * This is a much more efficient version of keyGetBaseName() and you should
 * use it if you are responsible enough to not mess up things.
 * @param key the object to obtain the basename from
 * @see keyGetBaseName(), keyGetBaseNameSize()
 * @ingroup keyname
 */
char *keyStealBaseName(const Key *key) {
	char *p=key->key;
	char *base=0;
	size_t size=0;
	
	while (*(p=keyNameGetOneLevel(p+size,&size))) base=p;
	
	if (base != key->key) return base;
	else return 0;
}


/******************************************* 
 *    General value manipulation methods   *
 *******************************************/




/**
 * An alias to keyGetValueSize().
 * 
 * @ingroup keyvalue
 */
ssize_t keyGetDataSize(const Key *key) {
	return keyGetValueSize(key);
}



/**
 * Returns the number of bytes needed to store the key value, including the
 * NULL terminator.
 *
 * This method is used with malloc() before a keyGetString() or keyGetBinary().
 *
 * @return the number of bytes needed to store the key value
 * @see keyGetString(), keyGetBinary(), keyStealValue()
 * @ingroup keyvalue
 */
ssize_t keyGetValueSize(const Key *key) {
	return key->dataSize;
}



/**
 * Set the value for @p key as @p newStringValue.
 * The function will allocate and save a private copy of @p newStringValue, so
 * the parameter can be freed after the call. 
 *
 * String values will be saved in backend storage, when kdbSetKey() will be
 * called, in UTF-8 universal encoding,regardeless of the program's current
 * encoding.
 *
 * @param key the key to set the string value
 * @param newStringValue NULL-terminated text string to be set as @p key's
 * 	value
 * @return the number of bytes actually saved in private struct including final
 * 	NULL
 * @see keyGetString(), keyStealValue()
 * @ingroup keyvalue
 */
ssize_t keySetString(Key *key, const char *newStringValue) {
	ssize_t ret=newStringValue?strblen(newStringValue):0;

	if (!newStringValue || !ret) ret=keySetRaw(key,0,0);
	else ret=keySetRaw(key,newStringValue,ret);

	keySetType(key,KEY_TYPE_STRING);

	return ret;
}



/**
 * Get the value of a key as a string.
 * If the value can't be represented as a text string (binary value, see
 * keyIsBin()), @c errno is set to KDBErr::KDB_RET_TYPEMISMATCH.
 *
 * @par Example:
 * @code
Key *key;
char buffer[300];

// populate key somehow...

if (keyIsBin(key)) keyGetBinary(key,buffer,sizeof(buffer));
else keyGetString(key,buffer,sizeof(buffer));
 * @endcode
 *
 * @param key the object to gather the value
 * @param returnedString pre-allocated memory to store a copy of the key value
 * @param maxSize number of bytes of pre-allocated memory in @p returnedString
 * @return the number of bytes actually copied to @p returnedString, including
 * 	final NULL
 * @see keyStealValue(), keySetString(), keyGetBinary()
 * @ingroup keyvalue
 */
ssize_t keyGetString(const Key *key, char *returnedString, size_t maxSize) {
	if (!key->data) {
		*returnedString=0;
		errno=KDB_RET_NODATA;
		return 0;
	}

	if (key->dataSize > maxSize) {
		errno=KDB_RET_TRUNC;
		return -1;
	}

	if (key->type < KEY_TYPE_STRING) {
		errno=KDB_RET_TYPEMISMATCH;
		return -1;
	}

	strcpy(returnedString,key->data);
	return key->dataSize;
}






/**
 * Return a pointer to the real internal @p key value.
 * This is a much more efficient version of keyGetString(), keyGetLink(),
 * keyGetBinary(), and you should use it if you are responsible enough to
 * not mess up things.
 * 
 * If @p key is not binary (keyIsBin()), you may cast the returned as a
 * @c "char *" because you'll get a NULL terminated regular string.
 * If it is binary, the size of the value can be determined by
 * keyGetValueSize().
 * 
 * Note that the Key structure also has as data size field that is calculated
 * by library internal calls to keySetRaw(), so to avoid inconsistencies, you
 * must never used the pointer returned by keyStealValue() method to set a new
 * value. Use keySetString(), keySetBinary(), keySetLink(), keySetRaw()
 * instead.
 * 
 * @par Example:
 * @code
KeySet *ks=ksNew();
Key *current=0;

kdbGetChildKeys("system/sw/my",ks,KDB_O_SORT|KDB_O_RECURSIVE);

ksRewind(ks);
while(current=ksNext(ks)) {
	size_t size=0;
	
	if (keyIsBin(current)) {
		size=keyGetValueSize(current);
		printf("Key %s has a value of size %d bytes. Value: <BINARY>\nComment: %s",
			keyStealName(current),
			size,
			keyStealComment(current));
	} else {
		size=strblen((char *)keyStealValue(current));
		printf("Key %s has a value of size %d bytes. Value: %s\nComment: %s",
			keyStealName(current),
			size,
			(char *)keyStealValue(current),
			keyStealComment(current));
	}
}
 * @endcode
 *
 * @param key the key object to work with
 * @see keyGetValueSize(), keyGetString(), keyGetBinary(), keyGetLink()
 * @ingroup keyvalue
 */
void *keyStealValue(const Key *key) {
	return key->data;
}








/**
 * Set @p key as type KeyType::KEY_TYPE_LINK with target @p target.
 * 
 * @param key the object to work with
 * @param target the value to set to @p key
 * @return whatever returned by keySetRaw()
 * @ingroup keyvalue
 *
 */
ssize_t keySetLink(Key *key, const char *target) {
	ssize_t ret=target?strblen(target):0;

	if (!target || !ret) ret=keySetRaw(key,0,0);
	else ret=keySetRaw(key,target,ret);

	keySetType(key,KEY_TYPE_LINK);

	return ret;
}



/**
 * Get the target key pointed by @p key.
 * 
 * @param returnedTarget a pre-allocated buffer to store the target
 * @param maxSize the size in bytes of the @p returnedTarget buffer
 * @param key the link key
 * @deprecated link system not perfect that way
 * @return the size in bytes of the copied target string
 * @ingroup keyvalue
 *
 */
ssize_t keyGetLink(const Key *key, char *returnedTarget, size_t maxSize) {
/**TODO: Remove or:
 * - update Doc
 * - add keyGetLinkSize()*/
	
	if (!key->data) {
		errno=KDB_RET_NODATA;
		return 0;
	}

	if (key->type != KEY_TYPE_LINK) {
		errno=KDB_RET_TYPEMISMATCH;
		return -1;
	}

	if (key->dataSize > maxSize) {
		errno=KDB_RET_TRUNC;
		return -1;
	}

	strcpy(returnedTarget,key->data);
	return key->dataSize;
}



/**
 * Check if a key is a link key
 *
 * The value of link keys is the key they point to.
 *
 * @return 1 if key is a link, 0 otherwise
 * @see keyIsDir(), keyGetType()
 * @ingroup keytest
 */
int keyIsLink(const Key *key) {
	return (key->type==KEY_TYPE_LINK);
}



/**
 * Check if a key is folder key
 *
 * Folder keys have no value.
 *
 * @return 1 if key is a folder, 0 otherwise
 * @see keyIsLink(), keyGetType()
 * @ingroup keytest
 */
int keyIsDir(const Key *key) {
	return (key->access & 0040000); /*S_IFDIR*/
	/* return key->type==KEY_TYPE_DIR; */
}



/**
 * Check if a key is of some binary type
 *
 * @return 1 if @link KeyType::KEY_TYPE_BINARY KEY_TYPE_BINARY @endlink <= type < @link KeyType::KEY_TYPE_STRING KEY_TYPE_STRING @endlink, 0 otherwise
 * @see keyGetBinary(), keySetBinary()
 * @ingroup keytest
 */
int keyIsBin(const Key *key) {
	return (KEY_TYPE_BINARY <= key->type && key->type < KEY_TYPE_STRING);
}


/**
 * Check if a key is of some string type
 *
 * @return 1 if type >= @link KeyType::KEY_TYPE_STRING KEY_TYPE_STRING @endlink, 0 otherwise
 * @see keyGetString(), keySetString()
 * @ingroup keytest
 */
int keyIsString(const Key *key) {
	return (key->type >= KEY_TYPE_STRING);
}




/**
 * Get the binary or string value of @p key.
 *
 * @param returnedValue pre-allocated memory to store a copy of @p key's value
 * @param maxSize number of bytes of pre-allocated memory
 * @return the number of bytes actually copied to @p returnedValue
 * @see keySetBinary(), keyGetString(), keyStealValue(), keyIsBin()
 * @ingroup keyvalue
 */
ssize_t keyGetBinary(const Key *key, void *returnedValue, size_t maxSize) {
	if (!key->data) {
		errno=KDB_RET_NODATA;
		return 0;
	}

	if (key->dataSize > maxSize) {
		errno=KDB_RET_TRUNC;
		return -1;
	}

	memcpy(returnedValue,key->data,key->dataSize);
	return key->dataSize;
}



/**
 * Set the value of a key as a binary.
 * A private copy of @p newBinary will allocated and saved inside @p key,
 * so the parameter can be deallocated after the call.
 *
 * The @c filesys backend, when used through a kdbSetKey(), will make the
 * value be encoded into a human readable hex-digit text format.
 *
 * Consider using a string key instead.
 *
 * @param key the object on which to set the value
 * @param newBinary value octet stream
 * @param dataSize number of bytes to copy from @p newBinary
 * @return the number of bytes actually copied to internal struct storage
 * @see keyGetBinary(), keyIsBin(), keyGetString(), keyStealValue(),
 * 	keySetString()
 * @ingroup keyvalue
 */
ssize_t keySetBinary(Key *key, const void *newBinary, size_t dataSize) {
	ssize_t ret=keySetRaw(key,newBinary,dataSize);

	keySetType(key,KEY_TYPE_BINARY);

	return ret;
}



/**
 * Test if the in-memory @p key object was changed after retrieved from disk.
 * All library methods that change Key properties take care of setting a 'key
 * is dirty' internal flag, that is checked by this method.
 *
 * @return 1 if @p key was changed in memory, 0 otherwise.
 * @ingroup keytest
 */
int keyNeedsSync(const Key *key) {
	/* if (!key) return 0; */
	return (key->flags & KEY_SWITCH_NEEDSYNC);
}



/**
 * Returns the key data type.
 *
 * @see keySetType(), keyIsBin(), keyIsDir(), keyIsLink()
 * @see #KeyType
 * @return the key type
 * @ingroup keyvalue
 *
 */
uint8_t keyGetType(const Key *key) {
	return key->type;
}



/**
 * Force a key type. See the #KeyType documentation to
 * understand the concepts behind Elektra key's value types.
 *
 * This method is usually not needed, unless you are working with more
 * semantic value types, or want to force a specific value type for a key.
 * It is not usually needed because the data type is automatically set
 * when setting the key value.
 *
 * @par Example:
 * @code
#define KEY_TYPE_COLOR (KEY_TYPE_STRING+4)

Key *color1;
Key *color2;

// Set color1 key
color1=keyNew("user/sw/MyApp/colors/someColor",
	KEY_SWITCH_TYPE,KEY_TYPE_COLOR,
	KEY_SWITCH_VALUE,"#4B52CA",
	KEY_SWITCH_COMMENT,"a custom color",
	KEY_SWITCH_END);

// Set color2 key
color2=keyNew("system/sw/MyApp/colors/green",
	KEY_SWITCH_TYPE,KEY_TYPE_COLOR,
	KEY_SWITCH_VALUE,"green",
	KEY_SWITCH_COMMENT,"the green color",
	KEY_SWITCH_END);

// Start affairs with Key database
kdbOpen();

// Commit the keys
kdbSetKey(color1);
kdbSetKey(color2);

// Reset memory related to our structures to reuse them later
keyClose(color1);
keyClose(color2);

// Retrieve keys from the database
keySetName(color1,"user/sw/MyApp/colors/someColor");
kdbGetKey(color1);

keySetName(color2,"system/sw/MyApp/colors/green");
kdbGetKey(color2);

// End of the affairs with Key database by now
kdbClose();

// Get the key types, which should be our user-defined KEY_TYPE_COLOR
uint8_t tcolor1=keyGetType(color1);
uint8_t tcolor2=keyGetType(color2);

keyDel(color1);
keyDel(color2);
 * @endcode
 *
 * When using KeyType::KEY_TYPE_DIR, this method will not set access
 * permissions to the key. You'll have to set it manually after
 * keySetType(), calling keySetAccess() with appropriate permissions.
 * Or use the keySetDir().
 *
 * @see keyGetType()
 * @see keySetDir()
 * @see #KeyType
 * @return the new type
 * @ingroup keyvalue
 *
 */
uint8_t keySetType(Key *key,uint8_t newType) {
	mode_t mask=0;

	switch (newType) {
		case KEY_TYPE_DIR:
			mask=umask(0); umask(mask);
			keySetDir(key,mask);
			break;
		default:
			key->type=newType;
			/* key->access &= ~(0040000 | dirSwitch); */
			/*remove directory bits and dirSwitch*/
	}
	key->flags |= KEY_SWITCH_NEEDSYNC;
	return key->type;
}


/**
 * Force a key type to be KEY_TYPE_DIR and set permissions.
 *
 * This method is provided as a convenience to avoid separate calls
 * of keySetType() and keySetAccess() and the complexities of
 * calculating permissions from umask().
 *
 * @par This method should be used this way:
 * @code
Key *key=keyNew(KEY_SWITCH_END);
mode_t mask=umask(0);

// restore backup
umask(mask);

// set directory permissions based on my umask
keySetDir(key,mask);
 * @endcode
 * 
 * @param key the key to set type and permissions
 * @param customUmask the umask of current session
 * @return always KEY_TYPE_DIR
 * @see keySetUAccess()
 * @see keySetType()
 * @ingroup keyvalue
 */
uint8_t keySetDir(Key *key, mode_t customUmask) {
	mode_t dirSwitch=0111;
	
	/* key->type=KEY_TYPE_DIR; */
	key->access|=(dirSwitch & ~customUmask) | 0040000; /*S_IFDIR*/
	/* keySetRaw(key,0,0); remove data */
	
	key->flags |= KEY_SWITCH_NEEDSYNC;
	
	return KEY_TYPE_DIR;
}


/**
 * Set raw data as the value of a key.
 * If NULL pointers are passed, key value is cleaned.
 * This method will not change or set the key type, and should not be
 * used unless working with user-defined value types.
 *
 * @param newBinary array of bytes to set as the value
 * @param dataSize number bytes to use from newBinary, including the final NULL
 * @return The number of bytes actually set in internall buffer.
 * @see keySetType(), keySetString(), keySetBinary()
 * @ingroup keyvalue
 */
ssize_t keySetRaw(Key *key, const void *newBinary, size_t dataSize) {
	if (!dataSize || !newBinary) {
		if (key->data) {
			free(key->data);
			key->data=0;
		}
		key->dataSize = 0;
		key->flags &= ~(KEY_SWITCH_VALUE);
		key->flags |= KEY_SWITCH_NEEDSYNC;
		return 0;
	}

	key->dataSize=dataSize;
	if (key->data) {
		char *p=0;
		p=realloc(key->data,key->dataSize);
		if (NULL==p) return -1;
		key->data=p;
	} else {
		key->data=malloc(key->dataSize);
        }

	if (!key->data) return -1;

	memcpy(key->data,newBinary,key->dataSize);
	key->flags |= KEY_SWITCH_VALUE | KEY_SWITCH_NEEDSYNC;
	return key->dataSize;
}








/***************************************************** 
 *    General owner or domain manipulation methods   *
 *****************************************************/



/**
 * Set the user domain of a key. A user domain is a user name.
 *
 * A private copy is stored, so the passed parameter can be freed after
 * the call.
 *
 * @param userDomain the user domain (or user name)
 * @return the number of bytes copied
 * @see keySetName(), keyGetOwner(), keyGetFullName()
 * @ingroup keymeta
 */
ssize_t keySetOwner(Key *key, const char *userDomain) {
	ssize_t size;

	if (userDomain == 0) {
		if (key->userDomain) {
			free(key->userDomain);
			key->userDomain=0;
		}
		key->flags &= ~KEY_SWITCH_DOMAIN;
		return 0;
	}

	if ((size=strblen(userDomain)) > 0) {
		if (key->userDomain) {
			char *p=0;
			p=realloc(key->userDomain,size);
			if (NULL==p) {
				errno=KDB_RET_NOMEM;
				return -1;
			}
			key->userDomain=p;
		} else {
			key->userDomain=malloc(size);
		}
		if (!key->userDomain) return -1; /* propagate errno */

		strcpy(key->userDomain,userDomain);
		key->flags |= KEY_SWITCH_DOMAIN | KEY_SWITCH_NEEDSYNC;
		return size;
	} else if (key->userDomain) {
		free(key->userDomain);
		key->userDomain=0;
		key->flags &= ~(KEY_SWITCH_DOMAIN | KEY_SWITCH_NEEDSYNC);
	}
	return 0;
}



/**
 * Return the size of the user domain of the Key.
 * 
 * @return number of bytes
 * @see keyGetOwner()
 * @ingroup keymeta
 */
ssize_t keyGetOwnerSize(const Key *key) {
	if (!key->userDomain) {
		errno=KDB_RET_NODOMAIN;
		return 0;
	}

	return strblen(key->userDomain);
}



/**
 * Return the user domain of the key.
 * - Given @p user:someuser/..... return @p someuser
 * - Given @p user:some.user/.... return @p some.user
 * - Given @p user/.... return the current user
 *
 * Only @p user/... keys have user domains.
 * For @p system/... keys (that doesn't have user domains) nothing is returned.
 *
 * Although usually the same, the user domain of a key is not related to its
 * UID. User domains are related to WHERE the key is stored on disk, while
 * UIDs are related to access controls of a key.
 *
 * @param key the object to work with
 * @param returned a pre-allocated space to store the owner
 * @param maxSize maximum number of bytes that fit returned
 * @return number of bytes written to buffer
 * @see keySetName(), keySetOwner(), keyStealOwner(), keyGetFullName()
 * @ingroup keymeta
 */
ssize_t keyGetOwner(const Key *key, char *returned, size_t maxSize) {
	ssize_t bytes;

	if (!key->userDomain) {
		errno=KDB_RET_NODOMAIN;
		return 0;
	}

	if (maxSize < (bytes=strblen(key->userDomain))) {
		errno=KDB_RET_TRUNC;
		return -1;
	} else strcpy(returned,key->userDomain);
	return bytes;
}




/**
 * Return a pointer to the real internal @p key owner or user domain.
 * This is a much more efficient version of keyGetOwner() and you
 * should use it if you are responsible enough to not mess up things.
 * 
 * @param key the key object to work with
 * @see keyGetOwner(), keySetOwner()
 * @ingroup keymeta
 */
char *keyStealOwner(const Key *key) {
	return key->userDomain;
}






/********************************************* 
 *    General comment manipulation methods   *
 *********************************************/



/**
 * Set a comment for a key.
 *
 * A key comment is like a configuration file comment. It has no size limit.
 * A private copy will be stored.
 *
 * @param newComment the comment, that can be freed after this call.
 * @return the number of bytes copied
 * @see keyGetComment()
 * @ingroup keymeta
 */
ssize_t keySetComment(Key *key, const char *newComment) {
	ssize_t size;

	if (newComment && (size=strblen(newComment)) > 0) {
		if (key->flags & KEY_SWITCH_COMMENT) {
			char *p=0;
			p=realloc(key->comment,size);
			if (NULL==p) {
				errno=KDB_RET_NOMEM;
				return -1;
			}
			key->comment=p;
		} else {
			key->comment=malloc(size);
		}
		if (!key->comment) return -1;

		strcpy(key->comment,newComment);
		key->flags |= KEY_SWITCH_COMMENT | KEY_SWITCH_NEEDSYNC;
		return key->commentSize=size;
	} else if (key->flags & KEY_SWITCH_COMMENT) {
		free(key->comment);
		key->comment=0;
		key->flags &= ~(KEY_SWITCH_COMMENT | KEY_SWITCH_NEEDSYNC);
	}
	return key->commentSize=0;
}



/**
 * Calculates number of bytes needed to store a key comment, including
 * final NULL.
 *
 * Use this method to allocate memory to retrieve a key comment.
 *
 * @return number of bytes needed
 * @see keyGetComment(), keySetComment()
 * @ingroup keymeta
 */
ssize_t keyGetCommentSize(const Key *key) {
	if (!key->comment) {
		errno=KDB_RET_NODESC;
		return 0;
	}

	return strblen(key->comment);
}



/**
 * Get the key comment.
 *
 * A Key comment is pretty much as a comment in a text configuration file.
 *
 * @param returnedDesc pre-allocated memory to copy the comments to
 * @param maxSize number of bytes that will fit returnedDesc
 * @return number of bytes written
 * @see keyGetCommentSize(), keySetComment()
 * @ingroup keymeta
 */
ssize_t keyGetComment(const Key *key, char *returnedDesc, size_t maxSize) {
	ssize_t bytes;

	if (!key->comment) {
		errno=KDB_RET_NODESC;
		return 0;
	}

	bytes=strblen(strncpy(returnedDesc,key->comment,maxSize));
	if (maxSize < strblen(key->comment)) {
		errno=KDB_RET_TRUNC;
		return -1;
	}
	return bytes;
}


/**
 * Return a pointer to the real internal @p key comment.
 * This is a much more efficient version of keyGetComment() and you
 * should use it if you are responsible enough to not mess up things.
 * 
 * @param key the key object to work with
 * @see keyGetCommentSize()
 * @see keyStealValue() for and example
 * @ingroup keymeta
 */
char *keyStealComment(const Key *key) {
	return key->comment;
}




/*********************************************
 *    UID, GID and ACL bits methods          *
 *********************************************/



/**
 * Get the user ID of a key
 *
 * Although usually the same, the UID of a key is not related to its user
 * domain.
 *
 * @return the system's UID of the key
 * @see keyGetGID(), keySetUID(), keyGetOwner()
 * @ingroup keymeta
 */
uid_t keyGetUID(const Key *key) {
	return key->uid;
}



/**
 * Set the user ID of a key.
 *
 * Although usually the same, the UID of a key is not related to its user
 * domain.
 *
 * @return 0 on success
 * @see keySetGID(), keyGetUID(), keyGetOwner()
 * @ingroup keymeta
 */
int keySetUID(Key *key, uid_t uid) {
	key->uid=uid;
	key->flags |= KEY_SWITCH_UID | KEY_SWITCH_NEEDSYNC;

	return 0;
}



/**
 * Get the system's group ID of a key
 *
 * @return the system's GID of the key
 * @see keySetGID(), keyGetUID()
 * @ingroup keymeta
 */
gid_t keyGetGID(const Key *key) {
	return key->gid;
}



/**
 * Set the system's group ID of a key
 *
 * @return the system's GID of the key
 * @see keyGetGID(), keySetUID()
 * @ingroup keymeta
 */
int keySetGID(Key *key, gid_t gid) {
	key->gid=gid;
	key->flags |= KEY_SWITCH_GID | KEY_SWITCH_NEEDSYNC;

	return 0;
}



/**
 * Return the key filesystem-like access permissions.
 * @see keySetAccess()
 * @ingroup keymeta
 */
mode_t keyGetAccess(const Key *key) {
	return key->access;
}



/**
 * Set the key filesystem-like access permissions.
 * 
 * Use this method before calling keySetDir().
 * 
 * @param key the key to set access permissions
 * @param mode the access permissions as for chmod(2)
 * @see keyGetAccess()
 * @ingroup keymeta
 */
int keySetAccess(Key *key, mode_t mode) {
	key->access=mode;
	key->flags |= KEY_SWITCH_MODE | KEY_SWITCH_NEEDSYNC;

	return 0;
}





/**
 * Set the key filesystem-like access permissions based on umask.
 *
 * Use this method before calling keySetDir().
 *
 * @param key the key to set access permissions
 * @param umask the umask for file/key creation as returned by umask(3)
 * @see keyGetAccess()
 * @ingroup keymeta
 */
int keySetUAccess(Key *key, mode_t umask) {
	key->access=DEFFILEMODE & ~umask;

	key->flags |= KEY_SWITCH_MODE | KEY_SWITCH_NEEDSYNC;

	return 0;
}




/*********************************************
 *    Access times methods                   *
 *********************************************/


/**
 * Get last modification time of the key on disk.
 * @ingroup keymeta
 */
time_t keyGetMTime(const Key *key) {
	return key->mtime;
}



/**
 * Get last time the key data was read from disk.
 * @ingroup keymeta
 */
time_t keyGetATime(const Key *key) {
	return key->atime;
}



/**
 * Get last time the key was stated from disk.
 * @ingroup keymeta
 */
time_t keyGetCTime(const Key *key) {
	return key->ctime;
}






/*********************************************
 *    Other methods                          *
 *********************************************/



ssize_t keyGetRecordSize(const Key *key) {
	return key->recordSize;
}


/**
 * Return a pointer to the next key, if @p key is member of a KeySet.
 * Different from ksNext(), this call does not affect the @link ksCurrent() KeySet internal cursor @endlink.
 * @ingroup keymisc
 */
Key *keyNext(Key *key) {
	return key->next;
}








/**
 * Duplicate a key in memory.
 *
 * Both keys have to be initialized with keyInit(). If you have set any
 * dynamic allocated memory for @p dest, make sure that you keyClose() it before keyDup().
 *
 * All private attributes of the @p source key will be copied, including its
 * context on a KeySet, and nothing will be shared between both keys.
 *
 * Memory will be allocated as needed for dynamic properties as value, comment, etc.
 *
 * @param source has to be an initializised source Key
 * @param dest will be the new copy of the Key
 * @return 0 on success
 * @see keyClose(), keyInit()
 * @ingroup keybase
 */
int keyDup(const Key *source, Key *dest) {

	/* Copy the struct data, including the "next" pointer */
	*dest=*source;

	/* prepare to set dynamic properties */
	dest->key=
			dest->comment=
			dest->userDomain=
			dest->data=0;

	/* TODO: handle errors, mostly due to memory allocation */
	/* Set properties that need memory allocation */
	keySetName(dest,source->key);
	keySetOwner(dest,source->userDomain);
	keySetComment(dest,source->comment);
	keySetRaw(dest,source->data,source->dataSize);

	dest->flags=source->flags | KEY_SWITCH_NEEDSYNC;
	
	return 0;
}





/**
 * Compare 2 keys.
 *
 * The returned flags bit array has 1s (differ) or 0s (equal) for each key
 * meta info compared, that can be logically ORed using @c #KeySwitch flags.
 * The flags you can use are @link KeySwitch::KEY_SWITCH_TYPE KEY_SWITCH_TYPE
 * @endlink, @link KeySwitch::KEY_SWITCH_NAME KEY_SWITCH_NAME @endlink,
 * @link KeySwitch::KEY_SWITCH_VALUE KEY_SWITCH_VALUE @endlink,
 * @link KeySwitch::KEY_SWITCH_OWNER KEY_SWITCH_OWNER @endlink,
 * @link KeySwitch::KEY_SWITCH_COMMENT KEY_SWITCH_COMMENT @endlink,
 * @link KeySwitch::KEY_SWITCH_UID KEY_SWITCH_UID @endlink,
 * @link KeySwitch::KEY_SWITCH_GID KEY_SWITCH_GID @endlink,
 * @link KeySwitch::KEY_SWITCH_MODE KEY_SWITCH_MODE @endlink,
 * @link KeySwitch::KEY_SWITCH_NEEDSYNC KEY_SWITCH_NEEDSYNC @endlink and
 * @link KeySwitch::KEY_SWITCH_FLAG KEY_SWITCH_FLAG @endlink.
 *
 * @par A very simple example would be
 * @code
Key *key1, *key;
uint32_t changes;

// omited key1 and key2 initialization and manipulation

changes=keyCompare(key1,key2);

if (changes == 0) printf("key1 and key2 are identicall\n");

if (changes & KEY_SWITCH_VALUE)
	printf("key1 and key2 have different values\n");
 
if (changes & KEY_SWITCH_UID)
	printf("key1 and key2 have different UID\n");
 
 *
 * @endcode
 *
 * @return a bit array poiting the differences
 * @see ksCompare() for examples and more detailed description
 * @see #KeySwitch
 * @ingroup keytest
 * 
 * @par Example of very powerfull specific Key lookup in a KeySet:
 * @code
KeySet *ks=ksNew();
Key *base;
Key *current;
uint32_t match;
uint32_t interests;


kdbGetChildKeys(ks,"usr/sw/MyApp",KDB_O_RECURSIVE);

// assemble a key we'll use to compare our KeySet to
base=keyNew("user/sw/MyApp/some/deep/key",
	KEY_SWITCH_TYPE,KEY_TYPE_LINK,
	KEY_SWITCH_VALUE,"system/mtp/x",
	KEY_SWITCH_MODE,0654,
	KEY_SWITCH_END));

// we are interested only in key type and access permissions
interests=(KEY_SWITCH_TYPE | KEY_SWITCH_MODE);
	
ksRewind(ks);   // put cursor in the begining
while ((curren=ksNext(ks))) {
	match=keyCompare(current,base);
	
	if ((~match & interests) == interests)
		printf("Key %s has same type and permissions of base key",keyStealName(current));

	// continue walking in the KeySet....
}

// now we want same name and/or value and/or sync status
interests=(KEY_SWITCH_NAME | KEY_SWITCH_VALUE | KEY_SWITCH_NEEDSYNC);

// we don't really need ksRewind(), since previous loop achieved end of KeySet
ksRewind(ks);
while ((current=ksNext(ks))) {
	match=keyCompare(current,base);
	
	if ((~match & interests) == interests) {
		printf("Key %s has same name, value, and sync status
			of base key",keyStealName(current));
	}
	// continue walking in the KeySet....
}

keyDel(base);
ksDel(ks);
 * @endcode
 * 
 */
uint32_t keyCompare(const Key *key1, const Key *key2) {
	uint32_t ret=0;


	/* Compare these numeric properties */
	if (key1->uid != key2->uid)                    ret|=KEY_SWITCH_UID;
	if (key1->gid != key2->gid)                    ret|=KEY_SWITCH_GID;
	if (key1->type != key2->type)                  ret|=KEY_SWITCH_TYPE;
#if defined(S_IRWXU) && defined(S_IRWXG) && defined(S_IRWXO)
	if ((key1->access & (S_IRWXU|S_IRWXG|S_IRWXO)) !=
		(key2->access & (S_IRWXU|S_IRWXG|S_IRWXO))) ret|=KEY_SWITCH_MODE;
#else
	if ((key1->access) !=
		(key2->access)) ret|=KEY_SWITCH_MODE;
#endif

	/* Compare these string properties.
	   A lot of decisions because strcmp can't handle NULL pointers */
	if (key1->key && key2->key) {
		if (strcmp(key1->key,key2->key))            ret|=KEY_SWITCH_NAME;
	} else
		if (key1->key || key2->key)                 ret|=KEY_SWITCH_NAME;

	if (key1->comment && key2->comment) {
		if (strcmp(key1->comment,key2->comment))    ret|=KEY_SWITCH_COMMENT;
	} else
		if (key1->comment || key2->comment)         ret|=KEY_SWITCH_COMMENT;

	if (key1->userDomain && key2->userDomain) {
		if (strcmp(key1->userDomain,key2->userDomain)) ret|=KEY_SWITCH_DOMAIN;
	} else
		if (key1->userDomain || key2->userDomain)      ret|=KEY_SWITCH_DOMAIN;

	/* select and compare some flags */
	ret|=
		/* put in evidence the differences only */
		(key1->flags ^ key2->flags) &
		/* and compare with what we need */
		(KEY_SWITCH_FLAG | KEY_SWITCH_NEEDSYNC);
	
	/* Compare data */
	if (key1->dataSize != key2->dataSize)          ret|=KEY_SWITCH_VALUE;
	else if (memcmp(key1->data,key2->data,
			(key1->dataSize<=key2->dataSize?key1->dataSize:key2->dataSize)))
	                                               ret|=KEY_SWITCH_VALUE;

	return ret;
}






/*********************************************
 *    Textual XML methods                    *
 *********************************************/


/**
 * Prints an XML representation of the key.
 *
 * String generated is of the form:
 * @verbatim
	<key name="system/sw/xorg/Monitor/Monitor0/Name"
		type="string" uid="root" gid="root" mode="0660">

		<value>Samsung TFT panel</value>
		<comment>My monitor</comment>
	</key>@endverbatim



 * @verbatim
	<key parent="system/sw/xorg/Monitor/Monitor0" basename="Name"
		type="string" uid="root" gid="root" mode="0660">

		<value>Samsung TFT panel</value>
		<comment>My monitor</comment>
	</key>@endverbatim
 *
 * @param stream where to write output: a file or stdout
 * @param options Some #KDBOptions ORed:
 * - @p KDBOptions::KDB_O_NUMBERS \n
 *   Do not convert UID and GID into user and group names
 * - @p KDBOptions::KDB_O_CONDENSED \n
 *   Less human readable, more condensed output
 * - @p KDBOptions::KDB_O_FULLNAME \n
 *   The @p user keys are exported with their full names (including
 *   user domains)
 *
 * @see ksToStream()
 * @return number of bytes written to output
 * @ingroup keymisc
 */
ssize_t keyToStream(const Key *key, FILE* stream, unsigned long options) {
	return keyToStreamBasename(key,stream,0,0,options);
}





/**
 * Same as keyToStream() but tries to strip @p parentSize bytes from
 * @p key name if it matches @p parent .
 *
 * Taking the example from keyToStream(), if @p parent is
 * @c "system/sw/xorg", the generated string is of the form:
 * @verbatim
	<key basename="Monitor/Monitor0/Name"
		type="string" uid="root" gid="root" mode="0660">

		<value>Samsung TFT panel</value>
		<comment>My monitor</comment>
	</key>@endverbatim
 *
 * It usefull to produce more human readable XML output of a key when
 * it is being represented in a context that defines the parent key name.
 * For example:
 *
 * @verbatim
	<keyset parent="user/sw">
		<key basename="kdbedit"..../>
		<key basename="phototools"..../>
		<key basename="myapp"..../>
	</keyset>@endverbatim
 *
 * In the bove example, each @p @<key@> entry was generated by a call to 
 * keyToStreamBasename() having @c "user/sw" as @p parent .
 * 
 * This method is used when ksToStream() is called with
 * KDBOption::KDB_O_HIER option.
 *
 * @param parentSize the maximum size of @p parent that will be used.
 *        If 0, the entire @p parent will be used.
 * @param parent the string (or part of it, defined by @p parentSize ) that
 *        will be used to strip from the key name.
 * @return number of bytes written to output
 * @ingroup keymisc
 */
ssize_t keyToStreamBasename(const Key *key, FILE *stream, const char *parent,
		const size_t parentSize, unsigned long options) {
	ssize_t written=0;
	char buffer[800];
	struct passwd *pwd=0;
	struct group *grp=0;

	/* Write key name */
	if (parent) {
		/* some logic to see if we should print only the relative basename */
		int found;
		size_t skip=parentSize ? parentSize : strblen(parent)-1;

		found=memcmp(parent,key->key,skip);
		if (found == 0) {
			while (*(key->key+skip) == RG_KEY_DELIM) ++skip;

			if (*(key->key+skip) != 0) /* we don't want a null basename */
				written+=fprintf(stream,"<key basename=\"%s\"",
					key->key+skip);
		}
	}

	if (written == 0) { /* no "<key basename=..." was written so far */
		if (options & KDB_O_FULLNAME) {
			keyGetFullName(key,buffer,sizeof(buffer));
			written+=fprintf(stream,"<key name=\"%s\"", buffer);
		} else written+=fprintf(stream,"<key name=\"%s\"", key->key);
	}


	if (options & KDB_O_CONDENSED) written+=fprintf(stream," ");
	else written+=fprintf(stream,"\n     ");



	/* Key type */
	if (options & KDB_O_NUMBERS) {
		written+=fprintf(stream,"type=\"%d\"", key->type);
	} else {
		buffer[0]=0;

		switch (key->type) {
			case KEY_TYPE_STRING:
				strcpy(buffer,"string");
				break;
			case KEY_TYPE_BINARY:
				strcpy(buffer,"binary");
				break;
			case KEY_TYPE_LINK:
				strcpy(buffer,"link");
				break;
		/*	case KEY_TYPE_DIR:
				strcpy(buffer,"directory");
				break; */
			case KEY_TYPE_UNDEFINED:
				strcpy(buffer,"undefined");
				break;
		}
		if (buffer[0]) written+=fprintf(stream,"type=\"%s\"", buffer);
		else written+=fprintf(stream,"type=\"%d\"", key->type);
		
		written+=fprintf(stream," ");
		if (keyIsDir(key)) written+=fprintf(stream,"isdir=\"yes\"");
	}

#ifdef HAVE_PWD_H
	if (keyIsUser(key)) {
		struct passwd *domainPwd=0;
		int uidMatch,gidMatch;
		
		domainPwd=getpwnam(key->userDomain);
		pwd=getpwuid(key->uid);
		grp=getgrgid(key->gid);
		
		uidMatch=(key->uid == domainPwd->pw_uid);
		gidMatch=(key->gid == domainPwd->pw_gid);
		
		if (options & KDB_O_FULLUGID) {
			if (pwd && !(options & KDB_O_NUMBERS))
				written+=fprintf(stream," uid=\"%s\"",pwd->pw_name);
			else written+=fprintf(stream,   " uid=\"%d\"",key->uid);
		
			if (grp && !(options & KDB_O_NUMBERS))
				written+=fprintf(stream," gid=\"%s\"",grp->gr_name);
			else written+=fprintf(stream," gid=\"%d\"",key->gid);
		} else {
			if (!uidMatch) {
				if (pwd && !(options & KDB_O_NUMBERS))
					written+=fprintf(stream," uid=\"%s\"",pwd->pw_name);
				else written+=fprintf(stream,   " uid=\"%d\"",key->uid);
			}

			if (!gidMatch) {
				if (grp && !(options & KDB_O_NUMBERS))
					written+=fprintf(stream," gid=\"%s\"",grp->gr_name);
				else written+=fprintf(stream,   " gid=\"%d\"",key->gid);
			}
		}
	} else {
		if (!(options & KDB_O_NUMBERS)) {
			pwd=getpwuid(key->uid);
			grp=getgrgid(key->gid);
		}

		/* UID, GID, mode */
		if (pwd) written+=fprintf(stream," uid=\"%s\"",pwd->pw_name);
		else  written+=fprintf(stream,   " uid=\"%d\"",key->uid);

		if (grp) written+=fprintf(stream," gid=\"%s\"",grp->gr_name);
		else  written+=fprintf(stream,   " gid=\"%d\"",key->gid);
	}
#endif

#if defined(S_IRWXU) && defined(S_IRWXG) && defined(S_IRWXO)
	written+=fprintf(stream," mode=\"0%o\"",
		key->access & (S_IRWXU|S_IRWXG|S_IRWXO));
#else
	written+=fprintf(stream," mode=\"0%o\"",
		key->access);
#endif


	if (!key->data && !key->comment) { /* no data AND no comment */
		written+=fprintf(stream,"/>");
		if (!(options & KDB_O_CONDENSED))
			written+=fprintf(stream,"\n\n\n\n\n\n");
		
		return written; /* end of <key/> */
	} else {
		if (key->data) {
			if ((key->dataSize <= 16) &&
					key->type >= KEY_TYPE_STRING &&
					!strchr(key->data,'\n')) {

				/* we'll use a "value" attribute instead of a <value> node,
				   for readability, so the cut size will be 16, which is
				   the maximum size of an IPv4 address */

				if (options & KDB_O_CONDENSED) written+=fprintf(stream," ");
				else written+=fprintf(stream,"\n     ");
				
				written+=fprintf(stream,"value=\"%s\"",(char *)key->data);
				
				if (key->comment) written+=fprintf(stream,">\n");
				else {
					written+=fprintf(stream,"/>");
					if (!(options & KDB_O_CONDENSED))
						written+=fprintf(stream,"\n\n\n\n\n\n");
				
					return written;
				}
			} else { /* value is bigger than 16 bytes: deserves own <value> */
				written+=fprintf(stream,">");
				if (!(options & KDB_O_CONDENSED)) written+=fprintf(stream,"\n\n     ");
				
				written+=fprintf(stream,"<value>");
				if (key->type >= KEY_TYPE_STRING || key->type < KEY_TYPE_BINARY) {
					written+=fprintf(stream,"<![CDATA[");
					fflush(stream);
					/* must chop ending \0 */
					written+=write(fileno(stream),key->data,key->dataSize-1);
					written+=fprintf(stream,"]]>");
				} else {
					/* Binary values */
					char *encoded=malloc(3*key->dataSize);
					size_t encodedSize;

					written+=fprintf(stream,"\n");
					encodedSize=encode(key->data,key->dataSize,encoded);
					fflush(stream);
					written+=write(fileno(stream),encoded,encodedSize);
					free(encoded);
					written+=fprintf(stream,"\n");
				}
				/* fflush(stream); */
				written+=fprintf(stream,"</value>");
			}
		} else { /* we have no data */
			if (key->comment) {
				written+=fprintf(stream,">");
				if (!(options & KDB_O_CONDENSED))
					written+=fprintf(stream,"\n");
			} else {
				written+=fprintf(stream,"/>");
				if (!(options & KDB_O_CONDENSED))
					written+=fprintf(stream,"\n\n\n\n\n\n");
			
				return written;
			}
		}
	}

	if (!(options & KDB_O_CONDENSED)) {
		written+=fprintf(stream,"\n");
		if (key->comment) written+=fprintf(stream,"     ");
	}

	if (key->comment) {
		written+=fprintf(stream,"<comment><![CDATA[%s]]></comment>", key->comment);
		if (!(options & KDB_O_CONDENSED))
			written+=fprintf(stream,"\n");
	}

	written+=fprintf(stream,"</key>");

	if (!(options & KDB_O_CONDENSED))
		written+=fprintf(stream,"\n\n\n\n\n\n");

	return written;
}


/*
size_t keyGetSerializedSizeWithoutName(Key *key) {
	return sizeof(KeyInfo)+
		key->dataSize+
		key->groupSize+
		key->commentSize;
}
*/

/*
size_t keyGetSerializedSize(Key *key) {
	size_t totalSize;

	totalSize=(key->key)+1+keyGetSerializedSizeWithoutName(key);
	if (key->userDomain) totalSize+=strlen(key->userDomain)+1;
	return totalSize;
}*/





/*********************************************
 *    Flag manipulation methods              *
 *********************************************/




/**
 * Set a general flag in the Key.
 *
 * The flag has no semantics to the library, only to your application.
 * It is just a simple marker that you may use to put the key on a special
 * state that makes sense to your application.
 *
 * @see keyGetFlag(), keyClearFlag()
 * @return always 0
 * @ingroup keymeta
 */
int keySetFlag(Key *key) {
	key->flags|=KEY_SWITCH_FLAG;

	return 0;
}



/**
 * Clear the general flag in the Key
 *
 * The flag has no semantics to the library, only to your application.
 * It is just a simple marker that you may use to put the key on a special
 * state that makes sense to your application.
 *
 * @see keyGetFlag(), keySetFlag()
 * @return always 0
 * @ingroup keymeta
 */
int keyClearFlag(Key *key) {
	key->flags &= ~KEY_SWITCH_FLAG;

	return 0;
}



/**
 * Get the general flag from the Key.
 *
 * The flag has no semantics to the library, only to your application.
 * It is just a simple marker that you may use to put the key on a special
 * state that makes sense to your application.
 *
 * @see keySetFlag(), keyClearFlag()
 * @see keyNew() with KeySwitch::KEY_SWITCH_NEEDSYNC
 * @return 1 if flag is set, 0 otherwise
 * @ingroup keymeta
 */
int keyGetFlag(const Key *key) {
	return (key->flags & KEY_SWITCH_FLAG) ? 1:0;
}





/*********************************************************************
 *    Key instance construction and destruction methods              *
 *********************************************************************/


/**
 * Initializes the Key object with some default values.
 *
 * This function should not be used by backends or
 * applications, use keyNew() instead.
 *
 * keyInit() sets the key to a clear state. It uses
 * memset to clear the memory. The type of the key
 * is KeyType::KEY_TYPE_UNDEFINED afterwards.
 *
 * uid, gid and access masks are set with the current
 * values of your system.
 *
 * keyNew() and keyDel() are better ways to deal with initialization
 * than keyInit() and keyClose()
 *
 * @par Example
 * @code
Key *key=keyNew("system/some/name");
...
// Use the key
...
keyClose(key);
keyInit(key);
...
// Reuse the key
...
keyDel(key);
 * @endcode
 *
 * @see keyClose() to free the memory allocated within that function.
 * @see keyNew() and keyDel() for construction and destruction of Keys
 * 
 * @return always 0;
 * @ingroup keybase
 */
int keyInit(Key *key) {
	mode_t localUmask;
	
	memset(key,0,sizeof(Key));
	key->type=KEY_TYPE_UNDEFINED;
	
	/* If we lack the getuid() and getgid() functions we leave uid and gid at 0 */
	#if defined(HAVE_GETUID) && defined(HAVE_GETGID)
	key->uid=getuid();
	key->gid=getgid();
	#endif
	
	/* by default do this, but backends should take care to carefully set
	   access permissions based on their handle */
	localUmask=umask(0); umask(localUmask);
	keySetUAccess(key,localUmask);

	key->flags = KEY_SWITCH_INITIALIZED;

	return 0;
}




/**
 * Finishes the usage of a Key object.
 *
 * The key must be keyInit() before any attempt to close it.
 *
 * Frees all internally allocated memory like value, comment, and leave the
 * Key object ready to be keyInit()ed to reuse, or deallocated.
 *
 * All internal states of the key will be NULL. After this
 * process there is no information inside the key.
 * 
 * keyNew() and keyDel() are better ways to deal with initialization
 * than keyInit() and keyClose()
 *
 * @see keyInit() how to allocate internal memory and an example
 * @see keyNew() and keyDel() for construction and destruction of Keys
 *
 * @ingroup keybase
 * @return always 0;
 */
int keyClose(Key *key) {
	if (key->key) free(key->key);
	if (key->data) free(key->data);
	if (key->comment) free(key->comment);
	if (key->userDomain) free(key->userDomain);
	memset(key,0,sizeof(Key));
	return 0;
}


/**
 * Return a block of memory with the entire key serialized, including
 * metainfo, value, comment and full name.
 * Deallocate it with a simple free().
 *
 * @see keyUnserialize()
 * @ingroup keymisc
 */
void *keySerialize(Key *key) {
	size_t metaInfoSize=0;
	size_t fullNameSize=0;
	void *serialized=0;
	
	fullNameSize=keyGetFullNameSize(key);
	
	metaInfoSize = KEY_METAINFO_SIZE(key);
	
	key->recordSize=metaInfoSize + key->dataSize + key->commentSize
		+ fullNameSize;
	serialized=malloc(key->recordSize);
	memset(serialized,0,key->recordSize);
	
	/* First part: the metainfo */
	memcpy(serialized,key,metaInfoSize);
	
	/* Second part: the comment */
	if (key->comment) memcpy((char *)(serialized)+metaInfoSize,
			key->comment,key->commentSize);
	
	/* Third part: the value */
	if (key->data) memcpy((char *)(serialized)+metaInfoSize+key->commentSize,
			key->data,key->dataSize);
	
	/* Fourth part: the full key name */
	keyGetFullName(key,
		(char *)(serialized)+metaInfoSize+key->commentSize+key->dataSize,
		fullNameSize);
	
	return serialized;
}

/**
 * Given a membory block created by keySerialize(), unserialize it into
 * a Key structure and return it
 *
 * The @p serialized can be freed after this call, because memory will be
 * allocated for all elements of the new key;
 *
 * @see keySerialize()
 * @ingroup keymisc
 */
Key *keyUnserialize(const void *serialized) {
	Key *key=0;
	size_t metaInfoSize=0;
	
	if (!serialized) return 0;
	
	key=keyNew(KEY_SWITCH_END);
	
	/* First part: the metainfo */
	metaInfoSize = KEY_METAINFO_SIZE(key);
	memcpy(key,serialized,metaInfoSize);
	
	/* Second part: the comment */
	if (key->commentSize) {
		key->comment=malloc(key->commentSize);
		memcpy(key->comment,
			(char *)(serialized)+metaInfoSize,
			key->commentSize);
	}
	
	/* Third part: the value */
	if (key->dataSize) {
		key->data=malloc(key->dataSize);
		memcpy(key->data,
			(char *)(serialized)+metaInfoSize+key->commentSize,
			key->dataSize);
	}
	
	/* Fourth part: the full key name */
	keySetName(key,
		(char *)(serialized)+metaInfoSize+key->commentSize+key->dataSize);
	
	return key;
}

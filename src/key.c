/***************************************************************************
                          key.c  -  Methods for Key and KeySet manipulation
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
$LastChangedBy$

*/

#include <stdio.h>
#include <stdarg.h>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <langinfo.h>

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
	char *found=index(s,0);
	if (found) return found-s+1;
	return 0;
}

/**
 * A save method for realloc.
 * 
 * This function differs from realloc() because it
 * handels error situations right.
 * The function can also allocate new memory, but it
 * is not recommended.
 * It is also possible to free the memory, when setting
 * new_size to 0. This is not recommended too.
 * ptr must not be NULL, this case is checked.
 * *ptr must be a previous allocated memory or NULL,
 * otherwise realloc will sigfault, this case can't
 * be checked.
 * par example
 * code
long int max = 100;
long int * array;

if ( (array = malloc ((max+1) * sizeof(long int))) == NULL )
{
	fprintf (stderr, "out of memory\n");
	return 1;
}

scanf ("%ld", &max);
if (srealloc ((void **)&array, (max+1) * sizeof(long int)) < 0)
{
	fprintf (stderr, "out of memory\n");
	return 1;
}
 * endcode
 * @return 0 at success, -1 on error (no memory leak)
 * @ingroup backend
 */
inline int srealloc (void ** ptr, size_t new_size) {
	void * h;
	if (ptr == NULL)
		return -1;
	h = realloc (* ptr, new_size);
	if (h == NULL) {
		if (new_size > 0) // dont free() twice
			free (* ptr);
		return -1;
	}
	*ptr = h;
	return 0;
}

/**
 * @defgroup key Key :: Basic Methods
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
 * your current Environment.
 *
 * In some situations this idea fails. When you need exactly
 * the same value back without any interpretation of the
 * characters, there is keySetBinary(). If you use that, its
 * very likely that your Configuration is not according
 * to the standard. Also for Numbers, Booleans and Date you
 * should use keyGetString(). To do so, you might use strtod()
 * strtol() and then atol() or atof() to convert back.
 * 
 * A key may also be just a Link. Here you will also find
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
 * The simplest way to call it is with no tags, only a key name. See example
 * bellow.
 * 
 * The Key attribute tags are the following:
 * - KeySwitch::KEY_SWITCH_TYPE \n
 *   This tag requires 1 or 2 more parameters. The first is obviously the
 *   type. If the type is KeySwitch::KEY_TYPE_BINARY or any other binary-like
 *   user-defined type (see keySetType()), a second parameter is needed and
 *   is the size in bytes (size_t) of the data passed on the subsequent
 *   KeySwitch::KEY_SWITCH_VALUE parameter. You must use this tag before
 *   KeySwitch::KEY_SWITCH_VALUE, otherwise KeyType::KEY_TYPE_STRING is
 *   assumed.
 * - KeySwitch::KEY_SWITCH_VALUE \n
 *   Next parameter is a pointer to the value that will be set to the key.
 *   If no KeySwitch::KEY_SWITCH_TYPE was used before,
 *   KeySwitch::KEY_TYPE_STRING is assumed.
 * - KeySwitch::KEY_SWITCH_UID, @p KeySwitch::KEY_SWITCH_GID \n
 *   Next parameter is taken as the UID (uid_t) or GID (gid_t) that will
 *   be defined on the key. See keySetUID() and keySetGID().
 * - KeySwitch::KEY_SWITCH_MODE \n
 *   Next parameter is taken as access permissions (mode_t) to the key.
 *   See keySetAccess().
 * - KeySwitch::KEY_SWITCH_DOMAIN \n
 *   Next parameter is the user domain. See keySetOwner().
 * - KeySwitch::KEY_SWITCH_COMMENT \n
 *   Next parameter is a comment. See keySetComment().
 * - KeySwitch::KEY_SWITCH_NEEDSYNC \n
 *   Needs no extra parameter. Makes keyNew() retrieve the Key from the
 *   backend with kdbGetKey(). In the same keyNew() call you can use this
 *   tag in conjunction with any other, which will make keyNew() modify
 *   only some attributes of the retrieved key, and return it for you.
 *   Order of parameters do matter. If the internal call to kdbGetKey()
 *   failed, you'll still have a valid, but flaged, key.
 *   Check with keyGetFlag(), and @p errno. You will have to kdbOpen()
 *   before using keyNew() with this tag.
 * - KeySwitch::KEY_SWITCH_END \n
 *   Must be the last parameter passed to keyNew(). It is allways
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
	KEY_SWITCH_NEEDSYNC,                   // a key retrieved from storage
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
	KEY_SWITCH_TYPE,KEY_TYPE_BINARY,7      // key type and value size (because it is binary)
	KEY_SWITCH_DOMAIN,"root",              // owner (not uid) is root
	KEY_SWITCH_VALUE,"some data",          // value that will be truncated
	KEY_SWITCH_COMMENT,"value is truncated",
	KEY_SWITCH_UID,0,                      // root uid
	KEY_SWITCH_END));                      // end of args
	
ksAppend(ks,keyNew("user/env/alias/ls",    // a key we know we have
	KEY_SWITCH_NEEDSYNC,                   // retrieve from storage
	KEY_SWITCH_END));                      // do nothing more
	
ksAppend(ks,keyNew("user/env/alias/ls",    // same key
	KEY_SWITCH_NEEDSYNC,                   // retrieve from storage
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
 * @ingroup key
 * 
 */
Key *keyNew(const char *keyName, ...) {
	va_list va;
	Key *key;
	u_int32_t action=0;
	u_int8_t keyType=KEY_TYPE_UNDEFINED;
	u_int8_t keyTypeBinary=0; /* a boolean shortcut */
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
		
		action=va_arg(va,u_int32_t);
		while (action) {
			switch (action) {
				case KEY_SWITCH_TYPE:
					/* We are waiting for 1 or 2 parameters
					 * following this action */
					
					/* First is the type */
					keyType=(u_int8_t)va_arg(va,unsigned int);
					
					keyTypeBinary=(KEY_TYPE_BINARY <= keyType &&
						keyType < KEY_TYPE_STRING);
					
					keySetType(key,keyType);
					
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
						/* Binary val: we need first the size of the value */
						valueSize=va_arg(va,size_t);
						keySetRaw(key,va_arg(va,void *),valueSize);
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
				case KEY_SWITCH_DOMAIN:
					keySetOwner(key,va_arg(va,char *));
					break;
				case KEY_SWITCH_COMMENT:
					keySetComment(key,va_arg(va,char *));
					break;
				case KEY_SWITCH_NEEDSYNC: {
					int rc=0;
					rc=kdbGetKey(key);
					if (rc)
						/* Flag the key to indicate an error in
						 * kdbGetKey(). Propagated errno will indicate
						 * the error ocurred. */
						key->flags|=KEY_SWITCH_FLAG; /* same as keySetFlag(key) */
				} break;
			}
			action=va_arg(va,u_int32_t);
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
 * @ingroup key
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
 * You should allways follow the guidelines for key tree structure creation at
 * @ref rules.
 *
 * A private copy of the key name will be stored, and the @p newName
 * parameter can be freed after this call.
 *
 * @return size in bytes of this new key name, or 0 if @p newName is empty,
 * 	or if @p newName is invalid, in which case @c errno is set to
 * 	KDBErrr::KDB_RET_INVALIDKEY.
 * @param key the key object
 * @param newName the new key name
 * @see keyNew(), keySetOwner()
 * @see keyGetName(), keyGetFullName(), keyStealName()
 * @ingroup keyname
 */
ssize_t keySetName(Key *key, const char *newName) {
	size_t length;
	size_t rootLength, userLength, systemLength, userDomainLength;
	size_t keyNameSize=1; /* equal to length plus a space for \0 */
	
/*
	if (!key) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}
	if (!keyIsInitialized(key)) keyInit(key); */ /* commented for stupidity */

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

	/* Remove trailing  '/' if caller passed some */
	while (length && newName[length]==RG_KEY_DELIM) {
		length--;
	}

	rootLength=keyNameGetRootNameSize(newName);
	if (!rootLength) {
		errno=KDB_RET_INVALIDKEY;
		return -1;
	}
	userLength=sizeof("user")-1;
	systemLength=sizeof("system")-1;
	userDomainLength=rootLength-userLength-1;
	if (userDomainLength<0) userDomainLength=0;

	if (!strncmp("user",newName,userLength<length?userLength:length)) {
		/* handle "user*" */
		if (length > userLength) {
			/* handle "user?*" */
			if (*(newName+userLength)==':') {
				/* handle "user:*" */
				if (userDomainLength > 0) {
					key->userDomain=realloc(key->userDomain,userDomainLength+1);
					strncpy(key->userDomain,newName+userLength+1,userDomainLength);
					key->userDomain[userDomainLength]=0;
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

		key->key=realloc(key->key,keyNameSize);

		/* here key->key must have a correct size allocated buffer */
		if (!key->key) return -1;

		strcpy(key->key,"user");
		strncpy(key->key+userLength,newName+rootLength,length-rootLength);
		key->key[keyNameSize-1]=0;

		if (!key->userDomain) {
			size_t bsize=strblen(getenv("USER"));

			if (!bsize) {}
			else {
				key->userDomain=malloc(bsize);
				strncpy(key->userDomain,getenv("USER"),bsize);
			}
		}
		key->flags |= KEY_SWITCH_ISUSER;
		key->flags &= ~KEY_SWITCH_ISSYSTEM;
	} else if (!strncmp("system",newName,systemLength<length?systemLength:length)) {
		/* handle "system*" */
		if (length > systemLength && *(newName+systemLength)!=RG_KEY_DELIM) {
			/* handle when != "system/ *" */
			errno=KDB_RET_INVALIDKEY;
			return -1;
		}
		keyNameSize+=length;
		key->key=realloc(key->key,keyNameSize);

		/* here key->key must have a correct size allocated buffer */
		if (!key->key) return -1;

		strncpy(key->key,newName,length);
		key->key[keyNameSize-1]=0;
		
		key->flags |= KEY_SWITCH_ISSYSTEM;
		key->flags &= ~KEY_SWITCH_ISUSER;
	} else {
		/* Passed name is neither "system" or "user" */
		errno=KDB_RET_INVALIDKEY;
		return -1;
	}

	key->flags |= KEY_SWITCH_NAME | KEY_SWITCH_NEEDSYNC;

	return keyNameSize;
}

 

/**
 * Adds @p baseName to the current key name.
 *
 * Assumes that @p key is a directory. @p baseName is appended to it.
 * The function adds @c '/' if needed while concatenating. This means it does not
 * matter whether the current path has a trailing '/' or not. If there is
 * none, it becomes appended.
 * 
 * So if @p key has name @c "system/dir1/dir2" and this method is called with
 * @p baseName @c "mykey", the resulting key will have name
 * @c "system/dir1/dir2/mykey".
 *
 * @return the size in bytes of the new key name
 * @see keySetBaseName()
 * @ingroup keyname
 * 
 */
ssize_t keyAddBaseName(Key *key,const char *baseName) {
	size_t nameSize=0;
	size_t newSize=0;
	int ndelim=0;
	
	if (key->key) nameSize=strblen(key->key)-1;
	if (baseName) newSize=strblen(baseName);
	else return nameSize;
	
	if (newSize==0) return nameSize;
	
	if (key->key) {
		/* Remove trailing '/' if caller passed some */
		while (nameSize-2 && key->key[nameSize-1]==RG_KEY_DELIM &&
		       key->key[nameSize-2]!=RG_KEY_DELIM) {
			key->key[--nameSize]=0;
		}
		
		if (key->key[nameSize-1] != RG_KEY_DELIM) nameSize++;
		
		/* Remove all '/' in the begining of baseName */
		while (baseName[ndelim] && baseName[ndelim] == RG_KEY_DELIM) {
			newSize--;
			ndelim++;
		}
		
		/* Now we know the final key size */
		newSize+=nameSize;
		key->key=realloc(key->key,newSize);
		
		if (key->key[nameSize-1] != RG_KEY_DELIM && baseName[ndelim])
			strcat(key->key,"/");
		
		strcat(key->key,baseName+ndelim);
		
	} else return keySetName(key,baseName);
	
	return newSize;
}




/**
 * Sets @c baseName as the new basename for @c key.
 *
 * All text after the last @c '/' in the @p key keyname is erased and
 * @p baseName is appended.
 *
 * So if @p key has name @c "system/dir1/dir2/mykey" and this method is
 * called with @p baseName @c "herkey", the resulting key will have name
 * @c "system/dir1/dir2/herkey".
 *
 * @return the size in bytes of the new key name
 * @see keyAddBaseName()
 * @ingroup keyname
 * 
 */
ssize_t keySetBaseName(Key *key, const char *baseName) {
	size_t newSize=strblen(baseName);
	char *end;
	
	end=rindex(key->key,'/');
	
	if (end) {
		newSize+=end-key->key;
		end[1]=0;
		key->key=realloc(key->key,newSize);
		strcat(key->key,baseName);
		return newSize;
	} else return keySetName(key,baseName);
}




/**
 * Bytes needed to store the key name without user domain.
 *
 * @return number of bytes needed to store key name without user domain
 * @see keyGetName(), keyGetFullNameSize()
 * @ingroup keyname
 */
ssize_t keyGetNameSize(const Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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

	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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
 * Bytes needed to store the key name including user domain.
 *
 * @return number of bytes needed to store key name including user domain
 * @see keyGetFullName(), keyGetNameSize()
 * @ingroup keyname
 */
ssize_t keyGetFullNameSize(const Key *key) {
	size_t returnedSize;

	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

	if (!key->key) return 0;

	returnedSize=strblen(key->key);

	if (!strncmp("user",key->key,sizeof("user")-1) && key->userDomain)
		returnedSize+=strblen(key->userDomain);

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
	if (!strncmp("user",key->key,userSize)) {
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
	/* if (!key) return 0;
	if (!keyIsInitialized(key)) return 0; */

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
	/* if (!keyName) return 0;
	if (!strlen(keyName)) return 0; */

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
	/* if (!key) return 0;
	if (!keyIsInitialized(key)) return 0; */

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
	/* if (!keyName) return 0;
	if (!strlen(keyName)) return 0; */

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
	/* if (!key) return 0;
	if (!keyIsInitialized(key)) return 0; */

	return (key->flags & KEY_SWITCH_ISUSER)?1:0;
}



/**
 * Gets number of bytes needed to store root name of a key name
 *
 * Possible root key names are @p system, @p user or @p "user:someuser" .
 *
 * @return number of bytes needed without ending NULL
 * @param keyName the name of the key
 * @see keyGetRootNameSize()
 * @ingroup keyname
 */
ssize_t keyNameGetRootNameSize(const char *keyName) {
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

	return end-keyName;
}



/**
 * Gets number of bytes needed to store root name of a key.
 *
 * Possible root key names are @p system or @p user .
 * This method does not consider the user domain in @p user:username keys.
 *
 * @return number of bytes needed without the ending NULL
 * @see keyGetFullRootNameSize(), keyNameGetRootNameSize()
 * @ingroup keyname
 */
ssize_t keyGetRootNameSize(const Key *key) {
	/* if (!key) return 0;
	if (!keyIsInitialized(key)) return 0; */
	if (!key->key) return 0;

	return keyNameGetRootNameSize(key->key);
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
 * @return number of bytes needed without ending NULL
 * @see keyNameGetRootNameSize(), keyGetRootNameSize(), keyGetFullRootName()
 * @ingroup keyname
 */
ssize_t keyGetRootName(const Key *key, char *returned, size_t maxSize) {
	size_t size;

	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

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
	} else strncpy(returned,key->key,size);
	return size;
}



/**
 * Calculates number of bytes needed to store full root name of a key.
 *
 * Possible root key names are @p system, @p user or @p user:someuser.
 * In contrast to keyGetRootNameSize(), this method considers the user
 * domain part, and you should prefer this one.
 *
 * @return number of bytes needed without ending NULL
 * @see keyNameGetRootNameSize(), keyGetRootNameSize()
 * @ingroup keyname
 */
ssize_t keyGetFullRootNameSize(const Key *key) {
	size_t size=0;

	if (keyIsUser(key)) {
		if (key->userDomain) size=strblen(key->userDomain);
		else size=strblen(getenv("USER"));
	}

	return size+keyNameGetRootNameSize(key->key);
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
 * @return number of bytes written to @p returned without ending NULL
 * @see keyGetFullRootNameSize(), keyGetRootName()
 * @ingroup keyname
 */
ssize_t keyGetFullRootName(const Key *key, char *returned, size_t maxSize) {
	size_t size;
	size_t userSize;
	char *cursor;

	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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
	
	userSize = keyGetRootNameSize(key);
	strncpy(returned,key->key, userSize); /* copy "user" or "system" */
	if (keyIsUser(key)) {
		cursor = returned + userSize;
		*cursor = ':'; cursor++;
		if (key->userDomain)
			strncpy (cursor, key->userDomain, size - userSize);
		else
			strncpy (cursor, getenv("USER"),  size - userSize);
	}

	return size;
}






/**
 * Get the number of bytes needed to store this key's parent name without
 * the ending NULL.
 * 
 * @see keyGetParentName() for example
 * @ingroup keyname
 */
ssize_t keyGetParentNameSize(const Key *key) {
	char *parentNameEnd;

	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

	if (!key->key) {
		errno=KDB_RET_NOKEY;
		return 0;
	}

	/*
		user   (size=0)
		user/parent/base
		user/parent/base/ (size=sizeof("user/parent"))
	*/

	parentNameEnd=strrchr(key->key,RG_KEY_DELIM);

	if (!parentNameEnd || parentNameEnd==key->key) {
		/* handle NULL or /something */
		return 0;
	}

	/* handle system/parent/base/ */
	if ((parentNameEnd-key->key) == (strblen(key->key)-2)) {
		parentNameEnd--;
		while (*parentNameEnd!=RG_KEY_DELIM) parentNameEnd--;
	}

	return parentNameEnd - key->key;
}



/**
 * Copy this key's parent name into a pre-allocated buffer.
 *
 * @see keyGetParentNameSize()
 * @param returnedParent pre-allocated buffer to copy parent name to
 * @param maxSize number of bytes pre-allocated
 * @par Example:
 * @code
Key *key=keyNew("system/parent/base",KEY_SWITCH_END);
char *parentName;
size_t parentSize;

parentSize=keyGetParentNameSize(key);
parentName=malloc(parentSize+1);
keyGetParentName(key,parentName,parentSize+1);
 * @endcode
 * @ingroup keyname
 */
ssize_t keyGetParentName(const Key *key, char *returnedParent, size_t maxSize) {
	ssize_t parentSize;

	parentSize=keyGetParentNameSize(key);

	if (parentSize+1 > maxSize) {
		errno=KDB_RET_TRUNC;
		return 0;
	} else strncpy(returnedParent,key->key,parentSize);

	returnedParent[parentSize]=0; /* ending NULL */
	
	return parentSize;
}






/**
 * Calculates number of bytes needed to store a basename of a key name.
 * Key names that have only root names (e.g. @c "system" or @c "user"
 * or @c "user:domain" ) does not have basenames, thus the function will
 * return 0 bytes.
 *
 * Basenames are denoted as:
 * - @p system/some/thing/basename
 * - @p user:domain/some/thing/basename
 *
 * @return number of bytes needed without ending NULL
 * @see keyGetBaseNameSize()
 * @ingroup keyname
 */
ssize_t keyNameGetBaseNameSize(const char *keyName) {
	char *end;

	end=strrchr(keyName,RG_KEY_DELIM);
	if (end) return keyName+strblen(keyName)-1-end;
	else return 0;
}



/**
 * Calculates number of bytes needed to store basename of @p key.
 * Key names that have only root names (e.g. @c "system" or @c "user"
 * or @c "user:domain" ) does not have basenames, thus the function will
 * return 0 bytes.
 *
 * Basenames are denoted as:
 * - @c system/some/thing/basename
 * - @c user:domain/some/thing/basename
 *
 * @return number of bytes needed without ending NULL
 * @see keyNameGetBaseNameSize()
 * @ingroup keyname
 */
ssize_t keyGetBaseNameSize(const Key *key) {
	/* if (!key) return 0;
	if (!keyIsInitialized(key)) return 0; */
	if (!key->key) return 0;

	return keyNameGetBaseNameSize(key->key);
}



/**
 * Calculate the basename of a key name and put it in @p returned.
 *
 * Some examples:
 * - basename of @p system/some/keyname is @p keyname
 * - basename of @p "user/tmp/some key" is @p "some key"
 *
 * @param key the key to extract basename from
 * @param returned a pre-allocated buffer to store the basename
 * @param maxSize size of the @p returned buffer
 * @return number of bytes copied to @p returned, or 0 and @p errno is set
 * @see keyStealBaseName(), keyGetBaseNameSize()
 * @ingroup keyname
 */
ssize_t keyGetBaseName(const Key *key, char *returned, size_t maxSize) {
	ssize_t size;
	size_t keySize;

	/* if (!key) {
		errno=KDB_RET_NULLKEY;
		return 0;
	}

	if (!keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

	if (!(size=keyGetBaseNameSize(key))) {
		errno=KDB_RET_NOKEY;
		return 0;
	}

	keySize=strblen(key->key);

	if (maxSize < size) {
		strncpy(returned,key->key+keySize-size,maxSize);
		errno=KDB_RET_TRUNC;
		return size;
	} else strncpy(returned,key->key+keySize-size,size);

	return size;
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
	char *end=strrchr(key->key,RG_KEY_DELIM);
	
	if (end) return end++;
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
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

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
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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
 * @return the size in bytes of the copied target string
 * @ingroup keyvalue
 *
 */
ssize_t keyGetLink(const Key *key, char *returnedTarget, size_t maxSize) {
/**TODO: Remove or:
 * - update Doc
 * - add keyGetLinkSize()*/
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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
	/* if (!key) return 0;
	if (!keyIsInitialized(key)) return 0; */

	return (S_ISLNK(key->access) || (key->type==KEY_TYPE_LINK));
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
	/* if (!key) return 0;
	if (!keyIsInitialized(key)) return 0; */

	return (S_ISDIR(key->access) || (key->type==KEY_TYPE_DIR));
}



/**
 * Check if a key is of some binary type
 *
 * @return 1 if @link KeyType::KEY_TYPE_BINARY KEY_TYPE_BINARY @endlink <= type < @link KeyType::KEY_TYPE_STRING KEY_TYPE_STRING @endlink, 0 otherwise
 * @see keyGetBinary(), keySetBinary()
 * @ingroup keytest
 */
int keyIsBin(const Key *key) {
	/* if (!key) return 0;
	if (!keyIsInitialized(key)) return 0; */

	return (KEY_TYPE_BINARY <= key->type && key->type < KEY_TYPE_STRING);
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
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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
 * UNIX sysadmins don't like to deal with binary sand box data.
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
u_int8_t keyGetType(const Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return KEY_TYPE_UNDEFINED;
	} */

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
 * The KeyType::KEY_TYPE_DIR is the only type that has no value, so when
 * using this method to set to this type, the key value will be freed.
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
u_int8_t tcolor1=keyGetType(color1);
u_int8_t tcolor2=keyGetType(color2);

keyDel(color1);
keyDel(color2);
 * @endcode
 *
 * @see keyGetType()
 * @see #KeyType
 * @return the new type
 * @ingroup keyvalue
 *
 */
u_int8_t keySetType(Key *key,u_int8_t newType) {
	mode_t dirSwitch=0111;

	/* if (!key) {
		errno=KDB_RET_UNINITIALIZED;
		return KEY_TYPE_UNDEFINED;
	}
	if (!keyIsInitialized(key)) keyInit(key); */

	switch (newType) {
		case KEY_TYPE_DIR:
			key->type=KEY_TYPE_DIR;
			dirSwitch=umask(0); umask(dirSwitch);
			dirSwitch=0111 & ~dirSwitch;
			key->access|=dirSwitch | S_IFDIR;
			keySetRaw(key,0,0); /* remove data */
			break;
		default:
			key->type=newType;
			key->access &= ~(S_IFDIR | dirSwitch);
			key->flags |= KEY_SWITCH_NEEDSYNC;
	}
	return key->type;
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
	/* if (!key) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}
	if (!keyIsInitialized(key)) keyInit(key); */

	if (!dataSize || !newBinary) {
		if (key->data) {
			free(key->data);
			key->data=0;
		}
		key->flags &= ~(KEY_SWITCH_VALUE);
		key->flags |= KEY_SWITCH_NEEDSYNC;
		return 0;
	}

	key->dataSize=dataSize;
	if (key->data) key->data=realloc(key->data,key->dataSize);
	else key->data=malloc(key->dataSize);

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

	/* if (!key) {
		errno=KDB_RET_UNINITIALIZED; // KDB_RET_NULLKEY
		return -1;
	}
	if (!keyIsInitialized(key)) keyInit(key); */
	
	if (userDomain == 0) {
		if (key->userDomain) {
			free(key->userDomain);
			key->userDomain=0;
		}
		return 0;
	}

	if ((size=strblen(userDomain)) > 0) {
		if (key->userDomain) {
			key->userDomain=realloc(key->userDomain,size);
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
	
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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

	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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

	/* if (!key) {
		errno=KDB_RET_UNINITIALIZED; // KDB_RET_NULLKEY 
		return 0;
	} 
	if (!keyIsInitialized(key)) keyInit(key); */

	if (newComment && (size=strblen(newComment)) > 0) {
		if (key->flags & KEY_SWITCH_COMMENT) {
			key->comment=realloc(key->comment,size);
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
	/*  if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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

	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

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






ssize_t keyGetRecordSize(const Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

	return key->recordSize;
}


/**
 * Return a pointer to the next key, if @p key is member of a KeySet.
 * Different from ksNext(), this call does not affect the KeySet internal cursor.
 * @ingroup keymisc
 */
Key *keyNext(Key *key) {
	return key->next;
}








/**
 * Clone a key.
 *
 * All private attributes of the source key will be copied, including its
 * context on a KeySet, and nothing will be shared between both keys.
 * keyClose() will be used on @p dest key before the operation, and internal
 * buffers will be automatically allocated on @p dest.
 *
 * @param source the source key
 * @param dest the new copy of the key
 * @return 0 on success
 * @see keyClose(), keyInit()
 * @ingroup key
 */
int keyDup(const Key *source, Key *dest) {

	/* clear everything first */
	keyClose(dest);

	/* Copy the struct data, including the "next" pointer */
	*dest=*source;

	/* prepare to set dynamic properties */
	dest->key=
	dest->comment=
	dest->userDomain=
	dest->data=0;

	/* Set properties that need memory allocation */
	keySetName(dest,source->key);
	keySetComment(dest,source->comment);
	keySetOwner(dest,source->userDomain);
	keySetRaw(dest,source->data,source->dataSize);

	dest->flags=source->flags;

	return 0;
}



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
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

	/*if (!(key->flags & KEY_SWITCH_UID)) return KDB_RET_NOCRED;*/

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
	/* if (!key) return errno=KDB_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key); */

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
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

	/*if (!(key->flags & KEY_SWITCH_GID)) return KDB_RET_NOCRED;*/

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
	/* if (!key) return errno=KDB_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key); */

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
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

	/*if (!(key->flags & KEY_SWITCH_MODE)) return KDB_RET_NOCRED;*/

	return key->access;
}



/**
 * Set the key filesystem-like access permissions.
 * @param key the key to set access permissions
 * @param mode the access permissions as for chmod(2)
 * @see keyGetAccess()
 * @ingroup keymeta
 */
int keySetAccess(Key *key, mode_t mode) {
	/* if (!key) return errno=KDB_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key); */

	key->access=mode;
	key->flags |= KEY_SWITCH_MODE | KEY_SWITCH_NEEDSYNC;

	return 0;
}



/**
 * Get last modification time of the key on disk.
 * @ingroup keymeta
 */
time_t keyGetMTime(const Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */
	/*if (!(key->flags & KEY_SWITCH_TIME)) return KDB_RET_NOTIME;*/

	return key->mtime;
}



/**
 * Get last time the key data was read from disk.
 * @ingroup keymeta
 */
time_t keyGetATime(const Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */
	/*if (!(key->flags & KEY_SWITCH_TIME)) return KDB_RET_NOTIME;*/

	return key->atime;
}



/**
 * Get last time the key was stated from disk.
 * @ingroup keymeta
 */
time_t keyGetCTime(const Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */
	/*if (!(key->flags & KEY_SWITCH_TIME)) return KDB_RET_NOTIME;*/

	return key->ctime;
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
 * @return a bit array poiting the differences
 * @see ksCompare() for examples and more detailed description
 * @see #KeySwitch
 * 
 * @par Example of very powerfull specific Key lookup in a KeySet:
 * @ingroup keytest
 * @code
KeySet *ks=ksNew();
Key *base;
Key *current;
u_int32_t match;
u_int32_t interests;


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
u_int32_t keyCompare(const Key *key1, const Key *key2) {
	u_int32_t ret=0;


	/* Compare these numeric properties */
	if (key1->uid != key2->uid)                    ret|=KEY_SWITCH_UID;
	if (key1->gid != key2->gid)                    ret|=KEY_SWITCH_GID;
	if (key1->type != key2->type)                  ret|=KEY_SWITCH_TYPE;
	if ((key1->access & (S_IRWXU|S_IRWXG|S_IRWXO)) !=
		(key2->access & (S_IRWXU|S_IRWXG|S_IRWXO))) ret|=KEY_SWITCH_MODE;

	/* Compare these string properties.
	   A lot of decisions because strcmp can't handle NULL pointers */
	if (key1->key && key2->key) {
		if (strcmp(key1->key,key2->key))            ret|=KEY_SWITCH_NAME;
	} else {
		if (key1->key)                              ret|=KEY_SWITCH_NAME;
		else if (key2->key)                         ret|=KEY_SWITCH_NAME;
	}

	if (key1->comment && key2->comment) {
		if (strcmp(key1->comment,key2->comment))    ret|=KEY_SWITCH_COMMENT;
	} else {
		if (key1->comment)                          ret|=KEY_SWITCH_COMMENT;
		else if (key2->comment)                     ret|=KEY_SWITCH_COMMENT;
	}

	if (key1->userDomain && key2->userDomain) {
		if (strcmp(key1->userDomain,key2->userDomain)) ret|=KEY_SWITCH_DOMAIN;
	} else {
		if (key1->userDomain)                       ret|=KEY_SWITCH_DOMAIN;
		else if (key2->comment)                     ret|=KEY_SWITCH_DOMAIN;
	}

	/* compare and select some flags */
	ret|=(key1->flags ^ key2->flags) & (KEY_SWITCH_FLAG | KEY_SWITCH_NEEDSYNC);
	
	/* Compare data */
	if (memcmp(key1->data,key2->data,
			(key1->dataSize<=key2->dataSize?key1->dataSize:key2->dataSize)))
		ret|=KEY_SWITCH_VALUE;

	return ret;
}






/**
 * Prints an XML representation of the key.
 *
 * String generated is of the form:
 * @verbatim
	<key name="system/sw/XFree/Monitor/Monitor0/Name"
		type="string" uid="root" gid="root" mode="0660">

		<value>Samsung TFT panel</value>
		<comment>My monitor</comment>
	</key>@endverbatim

 * Accepted options that can be ORed:
 * @param stream where to write output: a file or stdout
 * @param options Some #KDBOptions ORed:
 * - @p KDBOptions::KDB_O_NUMBERS \n
 *   Do not convert UID and GID into user and group names
 * - @p KDBOptions::KDB_O_CONDENSED \n
 *   Less human readable, more condensed output
 *
 * @see ksToStream()
 * @return number of bytes written to output
 * @ingroup keymisc
 */
ssize_t keyToStream(const Key *key, FILE* stream, unsigned long options) {
	ssize_t written=0;
	char buffer[800];
	struct passwd *pwd=0;
	struct group *grp=0;


	/* if (!key || !keyIsInitialized(key) // || !key->key * / ) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

	/* Write key name */
	if (options & KDB_O_FULLNAME) {
		keyGetFullName(key,buffer,sizeof(buffer));
		written+=fprintf(stream,"<key name=\"%s\"", buffer);
	} else written+=fprintf(stream,"<key name=\"%s\"", key->key);


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
			case KEY_TYPE_DIR:
				strcpy(buffer,"directory");
				break;
			case KEY_TYPE_UNDEFINED:
				strcpy(buffer,"undefined");
				break;
		}
		if (buffer[0]) written+=fprintf(stream,"type=\"%s\"", buffer);
		else written+=fprintf(stream,"type=\"%d\"", key->type);
	}


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
			else  written+=fprintf(stream,   " gid=\"%d\"",key->gid);
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

	written+=fprintf(stream," mode=\"0%o\">",
		key->access & (S_IRWXU|S_IRWXG|S_IRWXO));



	if (!(options & KDB_O_CONDENSED) && (key->data || key->comment))
		written+=fprintf(stream,"\n\n     ");

	if (key->data) {
		written+=fprintf(stream,"<value>");
		fflush(stream);
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
			fflush(stream);
			encodedSize=encode(key->data,key->dataSize,encoded);
			written+=write(fileno(stream),encoded,encodedSize);
			fflush(stream);
			free(encoded);
			written+=fprintf(stream,"\n");
		}
		fflush(stream);
		written+=fprintf(stream,"</value>");
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






/**
 * Set a general flag in the Key.
 *
 * The flag has no semantics to the library, only to your application.
 *
 * @see keyGetFlag(), keyClearFlag()
 * @return allways 0
 * @ingroup keymeta
 */
int keySetFlag(Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

	key->flags|=KEY_SWITCH_FLAG;

	return 0;
}



/**
 * Clear the general flag in the Key
 *
 * The flag has no semantics to the library, only to your application.
 *
 * @see keyGetFlag(), keySetFlag()
 * @return allways 0
 * @ingroup keymeta
 */
int keyClearFlag(Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	} */

	key->flags &= ~KEY_SWITCH_FLAG;

	return 0;
}



/**
 * Get the flag from the Key.
 *
 * The flag has no semantics to the library, only to your application.
 *
 * @see keySetFlag(), keyClearFlag()
 * @see keyNew() with KeySwitch::KEY_SWITCH_NEEDSYNC
 * @return 1 if flag is set, 0 otherwise
 * @ingroup keymeta
 */
int keyGetFlag(const Key *key) {
	/* if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	} */

	return (key->flags & KEY_SWITCH_FLAG) ? 1:0;
}



 
 
/**
 * Initializes a previously allocated Key object.
 *
 * This function should not be used, use keyNew() instead.
 *
 * keyInit sets the key to a clear state. It uses
 * memset to clear the memory. The type of the key
 * is KEY_TYPE_UNDEFINED afterwards.
 *
 * uid, gid and access masks are set with the current
 * values of your system.
 *
 * @par example
 * code
key=(Key *)malloc(sizeof(Key));
if (!key) return 0;
keyInit(key);
 * endcode
 * @see keyNew(), keyDel(), keyClose()
 * @return allways 0;
 * @ingroup key
 */
int keyInit(Key *key) {
	/* if (!key) return errno=KDB_RET_NULLKEY; */

	memset(key,0,sizeof(Key));
	key->type=KEY_TYPE_UNDEFINED;
	key->uid=getuid();
	key->gid=getgid();
	key->access=umask(0); umask(key->access);
	key->access=DEFFILEMODE & ~key->access;

	key->flags = KEY_SWITCH_INITIALIZED;

	return 0;
}




/**
 * Finishes the usage of a Key object.
 *
 * Frees all internally allocated memory, and leave the Key object
 * ready to be keyInit()ed to reuse, or deallocated.
 * 
 * @see keyInit() for usage example
 * @see keyNew() and keyDel() as a more practicall approach for Key
 * 	construction and destruction
 * @ingroup key
 * @return allways 0;
 */
int keyClose(Key *key) {
	/* if (!key) return errno=KDB_RET_NULLKEY;
	if (!keyIsInitialized(key)) return 0; */

	free(key->key);
	free(key->data);
	free(key->comment);
	free(key->userDomain);
	memset(key,0,sizeof(Key));
	return 0;
}







/**
 * @defgroup keyset KeySet :: Class Methods
 * @brief Methods to manipulate KeySets.
 * A KeySet is a linked list to group a number of Keys.
 * Key Sets have an @link ksCurrent() internal cursor @endlink to help
 * in the Key navigation.
 *
 * These are the methods to make various manipulations in the objects of class KeySet.
 * Methods for @link ksSort() sorting @endlink, @link ksAppendKeys() merging
 * @endlink, @link ksCompare() comparing @endlink, and @link ksNext() internal
 * cursor manipulation @endlink are provided.
 * To use them:
 * @code
#include <kdb.h>
 * @endcode
 *
 * @{
 */

 
/**
 * Allocate, initialize and return a new KeySet object.
 * Objects created with ksNew() must be destroyed with ksDel().
 * 
 * Due to ABI compatibility, the @p KeySet structure is only declared in kdb.h,
 * and not defined. So you can only declare @p pointers to @p KeySets in your
 * program, and allocate and free memory for them with ksNew() and ksDel()
 * respectively.
 * See http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html#AEN135
 * 
 * @see ksDel()
 * @return a ready to use KeySet object
 */ 
KeySet *ksNew() {
	KeySet *new=(KeySet *)malloc(sizeof(KeySet));
	ksInit(new);
	return new;
} 



/**
 * A destructor for KeySet objects.
 * 
 * Cleans all internal dynamic attributes, keyDel() all contained Keys,
 * and free()s the release the KeySet object memory (that was previously
 * allocated by ksNew()). There is the @p ksFree() macro if you prefer this
 * method name.
 *
 * @see ksNew(), ksClose()
 * @return whatever is returned by ksClose()
 */
int ksDel(KeySet *ks) {
	int rc;
	
	rc=ksClose(ks);
	free(ks);
	
	return rc;
}



/**
 * Return the number of keys contained by @p ks.
 */
ssize_t ksGetSize(KeySet *ks) {
	return ks->size;
}








/******************************************* 
 *           KeySet walking methods        *
 *******************************************/


/**
 * Resets a KeySet internal cursor.
 * Use it to set the cursor to the begining of the KeySet
 *
 * @return allways 0
 * @see ksNext(), ksCurrent(), kdbMonitorKeys() for an example
 *
 */
int ksRewind(KeySet *ks) {
	ks->cursor=0;
	return 0;
}



/**
 * Returns the next Key in a KeySet.
 * KeySets have an internal cursor that can be reset with ksRewind(). Every
 * time ksNext() is called the cursor is incremented and the new current Key
 * is returned.
 * You'll get a NULL pointer if the end of KeySet was reached. After that,
 * if ksNext() is called again, it will set the cursor to the begining of
 * the KeySet and the first key is returned.
 *
 * @return the new current Key
 * @see ksRewind(), ksCurrent()
 *
 */
Key *ksNext(KeySet *ks) {
	if (ks->cursor) ks->cursor=ks->cursor->next;
	else ks->cursor=ks->start;

	return ks->cursor;
}



/**
 * Return the current Key
 *
 * @return pointer to the Key pointed by @p ks's cursor
 * @see ksNext(), ksRewind()
 * @see kdbMonitorKeys() for a usage example
 */
Key *ksCurrent(const KeySet *ks) {
	return ks->cursor;
}



/**
 * Return the first key in the KeySet, whithout changing the KeySet's
 * internal cursor.
 * 
 * @see ksTail(), ksCurrent(), ksNext()
 * @see ksRewind() which also resets the internal cursor.
 */
Key *ksHead(KeySet *ks) {
	return ks->start;
}



/**
 * Return the last key in the KeySet, whithout changing the KeySet's
 * internal cursor.
 * 
 * @see ksHead(), ksCurrent(), ksNext()
 */
Key *ksTail(KeySet *ks) {
	return ks->end;
}











/******************************************* 
 *    Looking up Keys inside KeySets       *
 *******************************************/


/**
 * Look for a Key contained in @p ks that matches @p name, starting from
 * ks' ksNext() position.
 * 
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor will not move, and a NULL pointer is
 * returned.
 * 
 * The @p ksLookup*() set of methods are designed to let you work with
 * entirely pre-loaded KeySets, so instead of kdbGetKey(), key by key, the
 * idea is to fully kdbGetChildKeys() for your application root key (which is
 * more performatic), and process it all at once with @p ksLookup*().
 *
 * @param ks where to look for
 * @param name key name you are looking for
 * @param options some @p KDB_O_* option bits. Currently suported:
 * 	- @p KDB_O_NOCASE @n
 * 	  Lookup ignoring case.
 * @return pointer to the Key found, 0 otherwise
 * @see ksLookupRE() for powerfull regular expressions based lookups
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByName(KeySet *ks, const char *name, unsigned long options) {
	Key *init=0;
	Key *current=0;
	size_t nameSize;
	size_t currentNameSize;
	
	nameSize=strblen(name);
	
	init=ks->cursor;
	
	while ((current=ksNext(ks))) {
		if (current->key == name) return current; /* for NULLs */
		
		currentNameSize=current->key?strblen(current->key):0;
		if (currentNameSize != nameSize) continue;
		
		if ((current->key && name)) {
			if ((options & KDB_O_NOCASE) &&
				!strcasecmp(current->key,name)) return current;
			else if (!strcmp(current->key,name)) return current;
		}
	}
	
	/* Reached end of KeySet. Put cursor in initial position. */
	ks->cursor=init;
	
	return 0;
}



/**
 * Lookup for a key which any of its @p where components matches the
 * @p regex regular expression.
 * 
 * @param ks the KeySet to lookup into
 * @param where any of @p KEY_SWITCH_NAME, @p KEY_SWITCH_VALUE,
 * 	@p KEY_SWITCH_OWNER, @p KEY_SWITCH_COMMENT ORed.
 * @param regexp a regcomp(3) pre-compiled regular expression
 * @param options some @p KDB_O_* ORed options to change lookup behavior.
 * 	Currently supported options:
 * - @p KDB_O_NOSPANPARENT @n
 *   Lookup only keys under ksCurrent()'s parent. If we are in the begining of
 *   the KeySet (ksCurrent()==NULL), this option is ignored. @p ks must be
 *   ksSort()ed (or kdbGetChildKeys() with @link KDBOption::KDB_O_SORT
 *   KDB_O_SORT @endlink) for this to work.
 * 
 * @return some of @p KEY_SWITCH_NAME, @p KEY_SWITCH_VALUE,
 * 	@p KEY_SWITCH_OWNER, @p KEY_SWITCH_COMMENT switches ORed to
 * 	indicate @p where the @p regex matched.
 * 
 * @see ksLookupByName(), ksLookupByValue(), keyCompare() for other types of
 * 	lookups.
 * @see kdbGetChildKeys(), ksSort()
 * 
 * @par Example:
 * @code
// This code will process all Devices options, device by device.
// Look how we use nested loops, palying with KDB_O_NOSPANPARENT.
// We can do more interesting things when playing with 2 or more regular
// expressions.

regex_t regex;

// you are NOT seeing spaces in this regex
regcomp(&regex,".* /InputDevices/.* /Options/.*",REG_NOSUB);
where=KEY_SWITCH_NAME; // look for it only in key names

ksRewind(ks);
do {
	// show all keys that match this name, and are siblings of the first match
	match=ksLookupRE(ks,where,&regex,0);
	if (match) {
		// We found a device and its first option,
		
		processOption(ksCurrent(ks));
		
		// now process other options of this same device
		do {
			// fetch only the options with same parent with the
			// help of KDB_O_NOSPANPARENT
			match=ksLookupRE(ks,where,&regex,KDB_O_NOSPANPARENT);
			
			if (match) processOption(ksCurrent(ks));
		} while (match);
	}
} while (match);

regfree(&regex); // free regex resources
 * @endcode
 * 
 * @par Examples of regular expressions:
 * @code
regex_t regex;

// The spaces between '*' and '/' and '*' chars are Doxygen mirages :)

regcomp(&regex,
	"some value .* more text",  // match this
	REG_NEWLINE | REG_NOSUB);   // all in a single line

regcomp(&regex,
	"Device/.* /Options/ *",      // only interested in option keys
	REG_ICASE | REG_NOSUB);      // ignore case

regcomp(&regex,
	"^system/folder/.* /basename$", // match real system/ keys that end with 'basename'
	REG_NOSUB);       // allways use REG_NOSUB to increase performance

regcomp(&regex,
	"^system/sw/xorg/.* /Screen[0-9]* /Displays/[0-9]* /Depth$", // we want all X.org's depths of all displays of all screens
	REG_ICASE | REG_NOSUB);   // we don't care about the case


regfree(&regex);        // don't forget to free resources

 * @endcode
 */
u_int32_t ksLookupRE(KeySet *ks, u_int32_t where,
		const regex_t *regexp, unsigned long options) {
	regmatch_t offsets;
	u_int32_t match=0;
	Key *init, *walker;
	char *parentName=0;
	size_t walkerNameSize=0,parentNameSize=0;
	
	init=ks->cursor;
	if (!init)
		/* I don't have a parent to match. Ignore this option. */
		options &= options & ~KDB_O_NOSPANPARENT;
	
	if (options & KDB_O_NOSPANPARENT) {
		/* User wants siblings. Prepare context. */
		parentNameSize=keyGetParentNameSize(init);
		parentName=(char *)malloc(parentNameSize);
		keyGetParentName(init,parentName,parentNameSize);
	}
	
	while ((walker=ksNext(ks))) {
		walkerNameSize=keyGetNameSize(walker);
		
		if (options & KDB_O_NOSPANPARENT) {
			/* User wants siblings. Check if walker is a sibling of init. */
			if (walkerNameSize < parentNameSize)
				/* we're out of out scope, so abort */
				break;
			
			if (memcmp(parentName,walker->key,parentNameSize))
				/* walker has a different parent, so abort */
				break;
		}
	
		if ((where & KEY_SWITCH_NAME) && walker->key)
			if (!regexec(regexp,walker->key,1,&offsets,0))
				match |= KEY_SWITCH_NAME;
		
		if ((where & KEY_SWITCH_VALUE) && walker->data &&
			!(KEY_TYPE_BINARY <= walker->type && walker->type < KEY_TYPE_STRING))
			if (!regexec(regexp,(char *)walker->data,1,&offsets,0))
				match |= KEY_SWITCH_VALUE;
		
		if ((where & KEY_SWITCH_OWNER) && keyIsUser(walker))
			if (!regexec(regexp,walker->userDomain,1,&offsets,0))
				match |= KEY_SWITCH_OWNER;
		
		if ((where & KEY_SWITCH_COMMENT) && walker->comment)
			if (!regexec(regexp,walker->comment,1,&offsets,0))
				match |= KEY_SWITCH_OWNER;
		
		if (match) return match;
	}
	
	if (parentName) free(parentName);
	ks->cursor=init;
	
	return 0;
}



/**
 * Lookup for a Key contained in @p ks KeySet that matches @p value,
 * starting from ks' ksNext() position.
 * 
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor won't move, and a NULL pointer is
 * returned.
 * 
 * This method jumps binary keys, unless @p value is NULL.
 * 
 * @par Example:
 * @code
ksRewind(ks);
while (key=ksLookupByValue(ks,"my value",0)) {
	// show all keys which value="my value"
	keyToStream(key,stdout,0);
}
 * @endcode
 * 
 * @param ks where to look for
 * @param value the value which owner key you want to find
 * @param options some @p KDB_O_* option bits. Currently supported:
 * 	- @p KDB_O_NOCASE @n
 * 	  Lookup ignoring case.
 * @return the Key found, 0 otherwise
 * @see ksLookupByBinaryValue()
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByValue(KeySet *ks, const char *value, unsigned long options) {
	Key *init=0;
	Key *current=0;
	size_t size=0;
	
	size=strblen(value);
	init=ks->cursor;
	
	while ((current=ksNext(ks))) {
		if (current->data == value) return current;
		
		if (size != current->dataSize) continue;
		
		if (KEY_TYPE_BINARY<= current->type && current->type < KEY_TYPE_STRING)
			continue;
		
		if ((current->data && value)) {
			if ((options & KDB_O_NOCASE) && 
				!strcasecmp(current->data,value)) return current;
			else if (!strcmp(current->data,value)) return current;
		}
	}
	
	/* reached end of KeySet */
	ks->cursor=init;
	
	return 0;
}



/**
 * Lookup for a Key contained in @p ks KeySet that matches the binary @p value,
 * starting from ks' ksNext() position.
 * 
 * If found, @p ks internal cursor will be positioned in the matched key
 * (also accessible by ksCurrent()), and a pointer to the Key is returned.
 * If not found, @p ks internal cursor won't move, and a NULL pointer is
 * returned.
 * 
 * @param ks where to look for
 * @param value the value which owner key you want to find
 * @param size the size of @p value
 * @param options some @p KDB_O_* option bits, for future use
 * @return the Key found, NULL otherwise
 * @see ksLookupByValue()
 * @see keyCompare() for very powerfull Key lookups in KeySets
 * @see ksCurrent(), ksRewind(), ksNext()
 */
Key *ksLookupByBinaryValue(KeySet *ks, void *value, size_t size,
		unsigned long options) {
	Key *init=0;
	Key *current=0;
	
	init=ks->cursor;
	
	while ((current=ksNext(ks))) {
		if (current->data == value) return current;
		
		if (size != current->dataSize) continue;
		
		if ((current->data && value) && 
				!memcmp(current->data,value,size)) return current;
	}
	
	/* reached end of KeySet, so reset cursor */
	ks->cursor=init;
	
	return 0;
}







/******************************************* 
 *           Filling up KeySets            *
 *******************************************/

 
/**
 * Insert a new Key in the begining of the KeySet. A reference to the key will
 * be stored, and not a copy of the key. So a future ksClose() or ksDel() on
 * @p ks will keyDel() the @p toInsert object.
 * The KeySet internal cursor is not moved.
 *
 * Do not ksInsert() Keys that are already members of other KeySets.
 *
 * @return the size of the KeySet after insertion
 * @param ks KeySet that will receive the key
 * @param toInsert Key that will be inserted into ks
 * @see ksAppend(), ksInsertKeys(), ksAppendKeys(), ksDel(), keyNew()
 *
 */
ssize_t ksInsert(KeySet *ks, Key *toInsert) {
	toInsert->next=ks->start;
	ks->start=toInsert;
	if (!ks->end) ks->end=toInsert;
	return ++ks->size;
}


/**
 * Remove and return the first key of @p ks. If @p ks' cursor was positioned
 * in the poped key, @p ks will be ksRewind()ed.
 * 
 * ksInsert() provides the 'push' bahavior.
 *
 * @return the first key of @p ks, or NULL if @p ks is empty
 * @param ks KeySet to work with
 * @see ksInsert(), ksRewind()
 * @see commandList() for an example
 *
 */
Key *ksPop(KeySet *ks) {
	Key *key=0;
	
	if (ks->start) {
		key=ks->start;
		ks->start=key->next;
		if (ks->end == key) ks->end=0;
		ks->size--;
		if (ks->cursor == key) ks->cursor=0;
	}
	
	return key;
}


/**
 * Remove and return the last key of @p ks. If @p ks' cursor was positioned
 * in the poped key, @p ks will be ksRewind()ed.
 * 
 * ksAppend() provides the 'push' bahavior.
 *
 * @return the last key of @p ks, or NULL if @p ks is empty
 * @param ks KeySet to work with
 * @see ksAppend(), ksRewind()
 * @see commandList() and ksCompare() for examples
 *
 */
Key *ksPopLast(KeySet *ks) {
	Key *key=0;
	Key *prev=0;
	
	/* When ks is empty: */
	if (ks->end == 0) return 0;
	
	/* When ks has only one member: */
	if (ks->start == ks->end) {
		key=ks->end;
		ks->cursor=ks->start=ks->end=0;
		ks->size=0;
		
		return key;
	}
	
	prev=ks->start;
	while (prev->next!=ks->end) prev=prev->next;
	
	key=ks->end;
	prev->next=0;
	ks->end=prev;
	ks->size--;
	if (ks->cursor == key) ks->cursor=0;
	
	return key;
}


/**
 * Transfers all keys from @p toInsert to the begining of @p ks.
 *
 * After this call, @p toInsert will be empty and can be deleted with ksDel().
 *
 * @return the size of the KeySet after insertion
 * @param ks the KeySet that will receive the keys
 * @param toInsert the KeySet that provides the keys that will be transfered
 * @see ksAppend(), ksInsert(), ksAppendKeys()
 *
 */
ssize_t ksInsertKeys(KeySet *ks, KeySet *toInsert) {
	if (toInsert->size) {
		toInsert->end->next=ks->start;
		ks->start=toInsert->start;

		ks->size+=toInsert->size;

		/* Invalidate the old KeySet */
		toInsert->start=toInsert->end=toInsert->cursor=0;
		toInsert->size=0;
	}
	return ks->size;
}



/**
 * Appends a new Key to the end of @p ks. A reference to the key will
 * be stored, and not a private copy. So a future ksClose() or ksDel() on
 * @p ks will keyDel() the @p toAppend object.
 * The KeySet internal cursor is not moved.
 *
 * Do not ksAppend() Keys that are already contained by other KeySets.
 *
 * @return the size of the KeySet after insertion
 * @param ks KeySet that will receive the key
 * @param toAppend Key that will be appended to ks
 * @see ksInsert(), ksInsertKeys(), ksAppendKeys(), keyNew(), ksDel()
 *
 */
ssize_t ksAppend(KeySet *ks, Key *toAppend) {
	toAppend->next=0;
	if (ks->end) ks->end->next=toAppend;
	if (!ks->start) ks->start=toAppend;
	ks->end=toAppend;
	return ++ks->size;
}



/**
 * Transfers all @p toAppend contained keys to the end of the @p ks.
 *
 * After this call, the @p toAppend KeySet will be empty, and can be
 * deleted with ksDel().
 *
 * @return the size of the KeySet after transfer
 * @param ks the KeySet that will receive the keys
 * @param toAppend the KeySet that provides the keys that will be transfered
 * @see ksAppend(), ksInsert(), ksInsertKeys()
 * 
 */
ssize_t ksAppendKeys(KeySet *ks, KeySet *toAppend) {
	if (toAppend->size) {
		if (ks->end) {
			ks->end->next=toAppend->start;
			ks->end=toAppend->end;
		} else {
			ks->end=toAppend->end;
			ks->start=toAppend->start;
		}

		ks->size+=toAppend->size;
		
		/* Invalidate the old KeySet */
		toAppend->start=toAppend->end=toAppend->cursor=0;
		toAppend->size=0;
	}
	return ks->size;
}










/******************************************* 
 *    Other operations                     *
 *******************************************/


/**
 * Compare 2 KeySets.
 *  
 * This method behavior is the following:
 * - A key (by full name) that is present on @p ks1 and @p ks2, and has
 *   something different, will be transfered from @p ks2 to @p ks1, and
 *   @p ks1's (old) version deleted.
 * - Keys present in @p ks1, but not in @p ks2 will be transfered from @p ks1
 *   to @p removed.
 * - Keys that are keyCompare() equal in @p ks1 and @p ks2 will be
 *   keyDel()eted from @p ks2.
 * - Keys present in @p ks2 but not in @p ks1 will
 *   be transfered to @p ks1.
 *
 * In the end, @p ks1 will have all the keys that matter, and @p ks2
 * will be empty.
 *
 * After ksCompare(), you should, in this order:
 * -# ksDel(ks2)
 * -# call kdbSetKeys() on @p ks1 to commit all changed keys
 * -# kdbRemoveKey() for all keys in the @p removed KeySet
 * -# ksDel(removed)
 *
 * @param ks1 first and main KeySet
 * @param ks2 second KeySet
 * @param removed (generally empty) KeySet that will be filled with keys
 * 	removed from @p ks1
 * @see keyCompare()
 * @see commandEdit() at the kdb command
 * @return allways 0
 * @par Example
 * @code
KeySet *ks1,*ks2,*removed;
Key *key;

ks1=ksNew();
ks2=ksNew();
removed=ksNew();

// ...
// Populate ks1 and ks2....
// ...

ksCompare(ks1,ks2,removed);

ksDel(ks2);  // second KeySet is allways empty after ksCompare()
kdbSetKeys(ks1); // commit changed keys
ksDel(ks1);  // don't need ks1 anymore

// Remove all keys that disapeared from ks1...
ksSort(removed); // Sort it first so then we ensure child keys are removed
                 // before their parents
while (key=ksPopLast(removed)) {
	kdbRemoveKey(key);
	keyDel(key);
}

ksDel(removed); // free the KeySet memory
 * @endcode
 */
int ksCompare(KeySet *ks1, KeySet *ks2, KeySet *removed) {
	int flagRemoved=1;
	Key *ks1Cursor=0;
	Key *ks2Cursor=0;

	Key *ks1PrevCursor=0;

	ks1Cursor=ks1->start;
	while (ks1Cursor) {
		Key *ks2PrevCursor=0;
		flagRemoved=1;
		
		for (ks2Cursor=ks2->start; ks2Cursor; ks2Cursor=ks2Cursor->next) {
			u_int32_t flags=keyCompare(ks1Cursor,ks2Cursor);
			
			if (!(flags & (KEY_SWITCH_NAME | KEY_SWITCH_DOMAIN))) {
				/* Comparing fullname-equal keys */
				flagRemoved=0; /* key was not removed */
					
				/* First remove from ks2 */
				if (ks2PrevCursor) ks2PrevCursor->next=ks2Cursor->next;
				else ks2->start=ks2Cursor->next;
				if (ks2->end==ks2Cursor) ks2->end=ks2PrevCursor;
				ks2->size--;
					
				if (flags) {
					/* keys are different. Transfer to ks1. */
					
					/* Put in ks1 */
					if (ks1PrevCursor) ks1PrevCursor->next=ks2Cursor;
					else ks1->start=ks2Cursor;
					if (ks1->end==ks1Cursor) ks1->end=ks2Cursor;
					ks2Cursor->next=ks1Cursor->next;
					
					/* delete old version */
					keyDel(ks1Cursor);
					
					/* Reset pointers */
					ks1Cursor=ks2Cursor;
				} else {
					/* Keys are identical. Delete ks2's key. */

					/* Delete ks2Cusrsor */
					keyDel(ks2Cursor);
				}
				/* Don't need to walk through ks2 anymore */
				break;
			}
			ks2PrevCursor=ks2Cursor;
			
		} /* ks2 iteration */
		
		if (flagRemoved) {
			/* This ks1 key was not found in ks2 */
			/* Transfer it from ks1 to removed */
			
			/* Remove from ks1 */
			if (ks1PrevCursor) ks1PrevCursor->next=ks1Cursor->next;
			else ks1->start=ks1Cursor->next;
			if (ks1->end==ks1Cursor) ks1->end=ks1PrevCursor;
			ks1->size--;

			/* Append to removed */
			ksAppend(removed,ks1Cursor);
			
			/* Reset pointers */
			if (ks1PrevCursor) ks1Cursor=ks1PrevCursor->next;
			else ks1Cursor=ks1->start;
		} else {
			ks1PrevCursor=ks1Cursor;
			ks1Cursor=ks1Cursor->next;
		}
	} /* ks1 iteration */
	
	/* Now transfer all remaining ks2 keys to ks1 */
	ksAppendKeys(ks1,ks2);
	
	return 0;
}




/**
 * Writes to @p stream an XML version of the @p ks object.
 *
 * String generated is of the form:
 * @verbatim
<?xml version="1.0" encoding="UTF-8"?>

<!-- Generated by Elektra API. Total of 7 keys. -->
  
<keyset xmlns="http://elektra.sourceforge.net"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://elektra.sourceforge.net elektra.xsd">


<key ...>...</key>
<key ...>...</key>
<key ...>...</key>

</keyset>@endverbatim
 *
 * @param stream where to write output: a file or stdout
 * @param options accepted #KDBOptions ORed:
 * - @p KDBOptions::KDB_O_NUMBERS \n
 *   Do not convert UID and GID into user and group names.
 * - @p KDBOptions::KDB_O_CONDENSED \n
 *   Less human readable, more condensed output.
 * - @p KDBOptions::KDB_O_XMLHEADERS \n
 *   Use it. Include the correct XML headers in the output. If not used, the
 *   <?xml?> and schema info inside the <keyset> object will not be generated.
 *
 * @see keyToStream()
 * @see commandList() for usage example
 * @return number of bytes written to output, or -1 if some error occurs
 */
ssize_t ksToStream(const KeySet *ks, FILE* stream, unsigned long options) {
	size_t written=0;
	Key *key=0;

	if (options & KDB_O_XMLHEADERS) {
		written+=fprintf(stream,"<?xml version=\"1.0\" encoding=\"%s\"?>\n",
			nl_langinfo(CODESET));
		written+=fprintf(stream,
			"\n\n<!-- Generated by Elektra API. Total of %d keys. -->\n\n\n\n",ks->size);
		
		written+=fprintf(stream,"<keyset xmlns=\"http://elektra.sourceforge.net\"\n\
        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n\
        xsi:schemaLocation=\"http://elektra.sourceforge.net elektra.xsd\">\n\n\n");
		
		/* written+=fprintf(stream,
			"<!DOCTYPE keyset PUBLIC \"-//Avi Alkalay//DTD Elektra 0.1.0//EN\" \"http://elektra.sf.net/dtd/elektra.dtd\">\n\n\n"); */
	} else written+=fprintf(stream,"<keyset>\n\n\n");

	for (key=ks->start; key; key=key->next)
		written+=keyToStream(key,stream,options);
	
	written+=fprintf(stream,"</keyset>\n");
	return written;
}



/* Used as a callback by the qsort() function */
int keyCompareByName(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;

	return strcmp(key1->key, key2->key);
}


/**
 * Sorts a KeySet aphabetically by Key name, using qsort().
 *
 * @param ks KeySet to be sorted
 */
void ksSort(KeySet *ks) {
	Key *keys[ks->size];
	Key *cursor;
	size_t c=0;

	if (ks->size) {
		for (cursor=ks->start; cursor; cursor=cursor->next, c++)
			keys[c]=cursor;

		qsort(keys,ks->size,sizeof(Key *),keyCompareByName);

		ks->start=cursor=keys[0];
		for (c=1; c<ks->size; c++) {
			cursor->next=keys[c];
			cursor=cursor->next;
		}
		cursor->next=0;
		ks->end=cursor;
	}
}






/**
 * KeySet object initializer.
 * 
 * You should always use ksNew() instead of ksInit().
 *
 * Every KeySet object that will be used must be initialized first, to setup
 * pointers, counters, etc. After use, all ksInit()ialized KeySets must be
 * cleaned with ksClose().
 * 
 * @see ksNew(), ksClose(), keyInit()
 * @return allways 0
 */
int ksInit(KeySet *ks) {
	ks->start=ks->end=ks->cursor=0;
	ks->size=0;
	
	return 0;
}


/**
 * KeySet object cleaner.
 *
 * Will keyDel() all contained keys, reset internal pointers and counters.
 * 
 * After this call, the @p ks object is ready to be freed by you.
 * 
 * @see keyDel(), ksInit(), keyClose()
 * @see ksAppend() for details on how keys are inserted in KeySets
 * @return allways 0
 */
int ksClose(KeySet *ks) {
	if (ks->size) {
		while (ks->size) {
			Key *destroyer=ks->start;
			ks->start=destroyer->next;
			keyDel(destroyer);
			--ks->size;
		}
	}
	ks->cursor=ks->end=ks->start;
	return 0;
}






/**
 * @} // end of KeySet group
 */

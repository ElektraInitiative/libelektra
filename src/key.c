/***************************************************************************
                          key.c  -  Methods for key manipulation
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

extern int errno;


/**
 * @return number of bytes used by the string, including the final NULL
 */
size_t strblen(const char *s) {
	char *found=index(s,0);
	if (found) return found-s+1;
	return 0;
}


/**
 * @defgroup key Key Class Methods
 * @brief The Key Class and its methods.
 *
 * To use it:
 * @code
#include <kdb.h>
 * @endcode
 *
 * A Key is the essential class that contains all key data and metadata.
 * Key properties are:
 * - Key name
 * - User domain
 * - Key value or data
 * - Data type
 * - Comment about the key
 * - UID, GID and access filesystem-like permissions
 * - Access, change and modification times
 * - A general flag
 *
 *
 * Rules for Key Names
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
 * Described here the methods to get and set, and make various manipulations
 * in the objects of class Key.
 *
 * @{
 */

 
 
 
/**
 * A practical way to fully create a Key object in one step.
 * This function tries to mimic the C++ way for constructors.
 *
 * You can call it in many different ways depending on the attribute tags you
 * pass as parameters. Tags are represented as the @p "enum KeySwitch" values,
 * and they tell keyNew() which Key attribute comes next.
 * 
 * The simplest way to call it is with no tags, only a key name. See example bellow.
 * 
 * The Key attribute tags are the following:
 * - @p KEY_SWITCH_TYPE \n
 *   This tag requires 1 or 2 more parameters. The first is obviously the
 *   type. If the type is KEY_TYPE_BINARY or any other binary-like
 *   user-defined type (see keySetType()), a second parameter is needed and
 *   is the size in bytes (size_t) of the data passed on the subsequent
 *   KEY_SWITCH_VALUE parameter. You must use this tag before
 *   KEY_SWITCH_VALUE, otherwise KEY_TYPE_STRING is assumed.
 * - @p KEY_SWITCH_VALUE \n
 *   Next parameter is a pointer to the value that will be set to the key.
 *   If no KEY_SWITCH_TYPE was used before, KEY_TYPE_STRING is assumed.
 * - @p KEY_SWITCH_UID, @p KEY_SWITCH_GID \n
 *   Next parametkeySetRawer is taken as the UID (uid_t) or GID (gid_t) that will
 *   be defined on the key. See keySetUID() and keySetGID().
 * - @p KEY_SWITCH_PRM \n
 *   Next parameter is taken as access permissions (mode_t) to the key.
 *   See keySetAccess().
 * - @p KEY_SWITCH_DOMAIN \n
 *   Next parameter is the user domain. See keySetOwner().
 * - @p KEY_SWITCH_COMMENT \n
 *   Next parameter is a comment. See keySetComment().
 * - @p KEY_SWITCH_NEEDSYNC \n
 *   Needs no extra parameter. Makes keyNew() retrieve the Key from the
 *   backend with kdbGetKey(). In the same keyNew() call you can use this
 *   tag in conjunction with any other, which will make keyNew() modify
 *   only some attributes of the retrieved key, and return it for you.
 *   Order of parameters do matter. If the internal call to kdbGetKey()
 *   was unssuccessful, you'll still have a valid, but flaged, key.
 *   Check with keyGetFlag(), and @p errno. You will have to kdbOpen()
 *   before using keyNew() with this tag.
 * - KEY_SWITCH_END \n
 *   Must be the last parameter passed to keyNew(). It is allways
 *   required, unless the @p keyName is NULL too.
 *   
 * @par Example:
 * @code
KeySet *ks=ksNew();

kdbOpen();
	
ksAppend(ks,keyNew(0));     // an empty key
	
ksAppend(ks,keyNew("user/sw",              // a simple key
	KEY_SWITCH_END));                      // no more args
	
ksAppend(&ks,keyNew("system/sw",
	KEY_SWITCH_NEEDSYNC,                   // a key retrieved from storage
	KEY_SWITCH_END));                      // end of args               
	
ksAppend(&ks,keyNew("user/tmp/ex1",
	KEY_SWITCH_VALUE,"some data",          // with a simple value
	KEY_SWITCH_END));                      // end of args
	
ksAppend(&ks,keyNew("user/tmp/ex2",
	KEY_SWITCH_VALUE,"some data",          // with a simple value
	KEY_SWITCH_PRM,0777,                   // permissions
	KEY_SWITCH_END));                      // end of args
	
ksAppend(&ks,keyNew("user/tmp/ex3",
	KEY_SWITCH_TYPE,KEY_TYPE_LINK,         // only type
	KEY_SWITCH_VALUE,"system/mtp/x",       // link destination
	KEY_SWITCH_PRM,0654,                   // weird permissions
	KEY_SWITCH_END));                      // end of args
	
ksAppend(&ks,keyNew("user/tmp/ex4",
	KEY_SWITCH_TYPE,KEY_TYPE_BINARY,7,     // type and value size
	KEY_SWITCH_COMMENT,"value is truncated",
	KEY_SWITCH_DOMAIN,"root",              // owner (not uid) is root
	KEY_SWITCH_VALUE,"some data",          // value that will be truncated
	KEY_SWITCH_UID,0,                      // root uid
	KEY_SWITCH_END));                      // end of args
	
ksAppend(&ks,keyNew("user/env/alias/ls",   // a key we know we have
	KEY_SWITCH_NEEDSYNC,                   // retrieve from storage
	KEY_SWITCH_END));                      // do nothing more
	
ksAppend(&ks,keyNew("user/env/alias/ls",   // same key
	KEY_SWITCH_NEEDSYNC,                   // retrieve from storage
	KEY_SWITCH_DOMAIN,"root",              // set new owner (not uid) as root
	KEY_SWITCH_COMMENT,"new comment",      // set new comment
	KEY_SWITCH_END));                      // end of args
	
ksToStream(ks,stdout,KDB_O_XMLHEADERS);
	
ksDel(ks);
kdbClose();
 * @endcode
 *
 * @see keyDel()
 * @return a pointer to a new allocated and initialized Key object
 */ 
Key *keyNew(const char *keyName, ...) {
	va_list va;
	Key *key;
	u_int32_t action=0;
	u_int8_t keyType=KEY_TYPE_UNDEFINED;
	size_t valueSize=0;
	
	key=(Key *)malloc(sizeof(Key));
	if (!key) return 0;
	keyInit(key);
	
	if (keyName) {
		size_t nameSize;
		
		nameSize=keySetName(key,keyName);
		if (! nameSize) keySetFlag(key);
		
		va_start(va,keyName);
		
		action=va_arg(va,u_int32_t);
		while (action) {
			switch (action) {
				case KEY_SWITCH_TYPE:
					/* We are waiting for 1 or 2 parameters
					 * following this action */
					
					/* First is the type */
					keyType=(u_int8_t)va_arg(va,unsigned int);
					
					if (keyType < KEY_TYPE_STRING && 
							keyType >= KEY_TYPE_BINARY)
						/* Second parameter is needed and is the valueSize */
						valueSize=va_arg(va,size_t);
					
					keySetType(key,keyType);
					
					break;
				case KEY_SWITCH_VALUE:
					if (keyType == KEY_TYPE_UNDEFINED)
						keyType=KEY_TYPE_STRING;

					
					if (keyType >= KEY_TYPE_STRING || 
							keyType < KEY_TYPE_BINARY) /* most popular cases */
						keySetString(key,va_arg(va,char *));
					else if (keyType < KEY_TYPE_STRING && 
							keyType >= KEY_TYPE_BINARY) /* binary types */
						keySetRaw(key,va_arg(va,void *),valueSize);

					
					if (keyType > KEY_TYPE_STRING ||
							keyType < KEY_TYPE_BINARY)
						/* reset the type due to the
						 * above keySetString override */
						keySetType(key,keyType);
					
					break;
				case KEY_SWITCH_UID:
					keySetUID(key,va_arg(va,uid_t));
					break;
				case KEY_SWITCH_GID:
					keySetUID(key,va_arg(va,gid_t));
					break;
				case KEY_SWITCH_PRM:
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
					if (rc) /* errno will be propagated */
						keySetFlag(key); /* just flag the key */
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
 * @see keyNew()
 * @return whatever is returned by keyClose()
 *
 */ 
int keyDel(Key *key) {
	int rc;
	
	rc=keyClose(key);
	free(key);
	
	return rc;
 }

 
int keyFree(Key *key) {
	return keyDel(key);
}
 
 
 
 
 
/**
 * Initializes a previously allocated Key object.
 *
 * You'll find the keyNew() function more usefull and straight forward
 * than keyInit().
 * 
 * Every Key object that will be used must be initialized first, to setup
 * pointers, counters, etc.
 * 
 * @par Example
 * @code
Key *key=keyNew("system/some/key",KEY_SWITCH_END);

// Do something with the key....

// Now reuse the memory allocated by keyNew()
keyClose(key);  // free all internal attributes first
keyInit(key);   // reinitialize it
// do something with key...
keyClose(key);
free(key);
 * @endcode
 * 
 * @see keyNew()
 * @see keyDel()
 * @see keyClose()
 */
int keyInit(Key *key) {
	if (!key) return errno=KDB_RET_NULLKEY;

	memset(key,0,sizeof(Key));
	key->type=KEY_TYPE_UNDEFINED;
	key->uid=getuid();
	key->gid=getgid();
	key->access=umask(0); umask(key->access);
	key->access=DEFFILEMODE & ~key->access;

	key->flags |= KEY_SWITCH_INITIALIZED | KEY_SWITCH_ACTIVE;

	return 0;
}




/**
 * Finishes the usage of a Key object.
 *
 * Frees all internally allocated memory, and leave the Key object
 * ready to be keyInit()ed to reuse, or deallocated.
 * 
 * @see keyInit() for usage example
 * @see keyNew() as a more usefull function
 * @see keyDel()
 */
int keyClose(Key *key) {
	if (!key) return errno=KDB_RET_NULLKEY;
	if (!keyIsInitialized(key)) return 0;

	free(key->key);
	free(key->data);
	free(key->comment);
	free(key->userDomain);
	memset(key,0,sizeof(Key));
	return 0;
}







/**
 * Test if a Key object is initialized.
 *
 * It is more or less reliable.
 * You'd better guarantee your code is robust enough using
 * keyInit() and keyClose() everytime.
 * @see keyInit()
 * @see keyClose()
 */
int keyIsInitialized(const Key *key) {
	if (!key) return 0;
	return ((key->flags & KEY_SWITCH_INITMASK)==KEY_SWITCH_INITIALIZED);
}



/**
 * Test if an in-memory Key object was changed after retrieved from disk.
 * All Key methods that change objects properties will set an internal flag,
 * that is checked by this method.
 *
 * @return 1 if the key was changed, 0 otherwise.
 */
int keyNeedsSync(const Key *key) {
	if (!key) return 0;
	return (key->flags & KEY_SWITCH_NEEDSYNC);
}



/**
 * Returns the key data type.
 *
 * @see keySetType()
 * @see KeyType
 * @return the key type
 *
 */
u_int8_t keyGetType(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return KEY_TYPE_UNDEFINED;
	}

	return key->type;
}


/**
 * Force a key type. See the @e "enum KeyType" documentation to
 * understand the concepts behind Elektra key's value types. 
 *
 * This method is usually not needed, unless you are working with more
 * semantic value types, or want to force a specific value type for a key.
 * It is not usually needed because the data type is automatically set
 * when setting the key value.
 *
 * The @p KEY_TYPE_DIR is the only type that has no value, so when
 * using this method to set to this type, the key value will be freed.
 *
 * @par Example:
 * @code
#define KEY_TYPE_COLOR KEY_TYPE_STRING+4

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
 * @see KeyType
 * @return the new type
 *
 */
u_int8_t keySetType(Key *key,u_int8_t newType) {
	mode_t dirSwitch=0111;

	if (!key) {
		errno=KDB_RET_UNINITIALIZED;
		return KEY_TYPE_UNDEFINED;
	}
	if (!keyIsInitialized(key)) keyInit(key);

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
 * Returns the number of bytes of the key value
 *
 * This method is used with malloc() before a keyGetString() or keyGetBinary().
 *
 * @return the number of bytes needed to store the key value
 * @see keyGetString()
 * @see keyGetBinary()
 */
size_t keyGetDataSize(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

	return key->dataSize;
}


size_t keyGetRecordSize(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

	return key->recordSize;
}




/**
 * Bytes needed to store the key name without user domain.
 *
 * @return number of bytes needed to store key name without user domain
 * @see keyGetName()
 * @see keyGetFullNameSize()
 */
size_t keyGetNameSize(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (key->key) return strblen(key->key);
	else return 0;
}




/**
 * Bytes needed to store the key name including user domain.
 *
 * @return number of bytes needed to store key name including user domain
 * @see keyGetFullName()
 * @see keyGetNameSize()
 */
size_t keyGetFullNameSize(const Key *key) {
	size_t returnedSize;

	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

	if (!key->key) return 0;

	returnedSize=strblen(key->key);

	if (!strncmp("user",key->key,sizeof("user")-1) && key->userDomain)
		returnedSize+=strblen(key->userDomain);

	return returnedSize;
}




Key *keyNext(Key *key) {
	return key->next;
}



/**
 * Get key full name, including the user domain name.
 *
 * @return number of bytes written
 * @param key the key object
 * @param returnedName pre-allocated memory to write the key name
 * @param maxSize maximum number of bytes that will fit in returnedName, including the final NULL
 */
size_t keyGetFullName(const Key *key, char *returnedName, size_t maxSize) {
	size_t userSize=sizeof("user")-1;
	size_t userDomainSize,length;
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
 * Get abreviated key name (without user domain name).
 *
 * @return number of bytes written
 * @param key the key object
 * @param returnedName pre-allocated memory to write the key name
 * @param maxSize maximum number of bytes that will fit in returnedName, including the final NULL
 */
size_t keyGetName(const Key *key, char *returnedName, size_t maxSize) {
	size_t bytes;

	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->key) {
		errno=KDB_RET_NOKEY;
		returnedName[0]=0;
		return 0;
	}

	bytes=strblen(strncpy(returnedName,key->key,maxSize));
	if (maxSize < strblen(key->key)) {
		errno=KDB_RET_TRUNC;
		return 0;
	}
	return bytes;
}





/**
 * Set a new name to a key.
 *
 * A valid name is of the form:
 * - @p system/something
 * - @p user/something
 * - @p user:username/something
 *
 * The last form has explicitly set the user domain, to let the library
 * know in which user folder to save the key. A user domain is a user name.
 * If not defined (the second form) current user is calculated and used
 * as default.
 *
 * A private copy of the key name will be stored, and the @p newName
 * parameter can be freed after this call.
 *
 * @return size in bytes of this new key name. When 0 is returned, or @p newName is empty, or something wrong happened and @p errno is propagated
 * @param key the key object
 * @param newName the new key name
 * @see keyNew()
 * @see keyGetName()
 * @see keyGetFullName()
 */
size_t keySetName(Key *key, const char *newName) {
	size_t length;
	size_t rootLength, userLength, systemLength, userDomainLength;
	size_t keyNameSize=1; /* equal to length plus a space for \0 */

	if (!key) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}
	if (!keyIsInitialized(key)) keyInit(key);

	/* handle null new key name, removing the old key */
	if (!newName || !(length=strblen(newName)-1)) {
		if (key->key) {
			free(key->key);
			key->key=0;
		}
		key->flags &= ~(KEY_SWITCH_NAME | KEY_SWITCH_NEEDSYNC);
		return 0;
	}

	/* Remove leading '/' if caller passed some */
	while (newName[length]==RG_KEY_DELIM) {
		length--;
	}

	rootLength=keyNameGetRootNameSize(newName);
	if (!rootLength) {
		errno=KDB_RET_INVALIDKEY;
		return 0;
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
				return 0;
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
		if (!key->key) return 0;

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

	} else if (!strncmp("system",newName,systemLength<length?systemLength:length)) {
		/* handle "system*" */
		if (length > systemLength && *(newName+systemLength)!=RG_KEY_DELIM) {
			/* handle when != "system/ *" */
			errno=KDB_RET_INVALIDKEY;
			return 0;
		}
		keyNameSize+=length;
		key->key=realloc(key->key,keyNameSize);

		/* here key->key must have a correct size allocated buffer */
		if (!key->key) return 0;

		strncpy(key->key,newName,length);
		key->key[keyNameSize-1]=0;
	} else {
		/* Passed name is neither "system" or "user" */
		errno=KDB_RET_INVALIDKEY;
		return 0;
	}

	key->flags |= KEY_SWITCH_NAME | KEY_SWITCH_NEEDSYNC;

	return keyNameSize;
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
 * @see keySetName()
 * @see keySetOwner()
 * @see keyGetFullName()
 */
size_t keyGetOwner(const Key *key, char *returned, size_t maxSize) {
	size_t bytes;

	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->userDomain) {
		errno=KDB_RET_NODOMAIN;
		return 0;
	}

	if (maxSize < (bytes=strblen(key->userDomain))) {
		errno=KDB_RET_TRUNC;
		return 0;
	} else strcpy(returned,key->userDomain);
	return bytes;
}

/**
 * Return the size of the user domain of the Key.
 * 
 * @return number of bytes
 * @see keyGetOwner()
 */
size_t keyGetOwnerSize(const Key *key) {
	
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->userDomain) {
		errno=KDB_RET_NODOMAIN;
		return 0;
	}

	return strblen(key->userDomain);
}


/**
 * Set the user domain of a key. A user domain is a user name.
 *
 * A private copy is stored, so the passed parameter can be freed after
 * the call.
 *
 * @param userDomain the user domain (or user name)
 * @return the number of bytes copied
 * @see keySetName()
 * @see keyGetOwner()
 * @see keyGetFullName()
 */
size_t keySetOwner(Key *key, const char *userDomain) {
	size_t size;

	if (!key) {
		errno=KDB_RET_UNINITIALIZED; /* KDB_RET_NULLKEY */
		return -1;
	}
	if (!keyIsInitialized(key)) keyInit(key);

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
 * Get the key comment.
 *
 * A Key comment is pretty much as a comment in a text configuration file.
 *
 * @param returnedDesc pre-allocated memory to copy the comments to
 * @param maxSize number of bytes that will fit returnedDesc
 * @return number of bytes written
 * @see keyGetCommentSize()
 * @see keySetComment()
 */
size_t keyGetComment(const Key *key, char *returnedDesc, size_t maxSize) {
	size_t bytes;

	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->comment) {
		errno=KDB_RET_NODESC;
		return 0;
	}

	bytes=strblen(strncpy(returnedDesc,key->comment,maxSize));
	if (maxSize < strblen(key->comment)) {
		errno=KDB_RET_TRUNC;
		return 0;
	}
	return bytes;
}



/**
 * Set a comment for a key.
 *
 * A key comment is like a configuration file comment. It has no size limit.
 * A private copy will be stored.
 *
 * @param newComment the comment, that can be freed after this call.
 * @return the number of bytes copied
 * @see keyGetComment()
 */
size_t keySetComment(Key *key, const char *newComment) {
	size_t size;

	if (!key) {
		errno=KDB_RET_UNINITIALIZED; /* KDB_RET_NULLKEY */
		return 0;
	}
	if (!keyIsInitialized(key)) keyInit(key);

	if (newComment && (size=strblen(newComment)) > 0) {
		if (key->flags & KEY_SWITCH_COMMENT) {
			key->comment=realloc(key->comment,size);
		} else {
			key->comment=malloc(size);
		}
		if (!key->comment) return 0;

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
 * @see keyGetComment()
 * @see keySetComment()
 */
size_t keyGetCommentSize(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->comment) {
		errno=KDB_RET_NODESC;
		return 0;
	}

	return strblen(key->comment);
}







/*

int keyGetInteger(Key *key, RG_DWORD *returnedInt) {
	if (!key || !keyIsInitialized(key))
		return errno=KDB_RET_UNINITIALIZED;

	if (!key->data)
		return errno=KDB_RET_NODATA;

	if (key->type > KEY_TYPE_DOUBLE)
		return errno=KDB_RET_TYPEMISMATCH;

	switch (key->type) {
		case KEY_TYPE_DOUBLE:
			*returnedInt=floor(*(double *)key->data);
			break;
		case KEY_TYPE_DWORD:
			*returnedInt=*(long long int *)key->data;
			break;
	}

	return 0;
}





size_t keySetInteger(Key *key, RG_DWORD newInt) {
	size_t ret;
	char number[60];

	ret=sprintf(number,"%d",newInt);

	if ((ret=keySetRaw(key,number,ret+1))<0) {
		return ret;
	}

	key->type=KEY_TYPE_STRING;

	return ret;
}
*/


/*

int keyGetDouble(Key *key, double *returnedDouble) {
	if (!key || !keyIsInitialized(key))
		return errno=KDB_RET_UNINITIALIZED;

	if (!key->data)
		return errno=KDB_RET_NODATA;

	if (key->type > KEY_TYPE_DOUBLE)
		return errno=KDB_RET_TYPEMISMATCH;

	switch (key->type) {
		case KEY_TYPE_DOUBLE:
			*returnedDouble=*(double *)key->data;
			break;
		case KEY_TYPE_DWORD:
			*returnedDouble=*(long long int *)key->data;
			break;
	}

	return 0;
}



size_t keySetDouble(Key *key, double newDouble) {
	size_t ret;

	if ((ret=keySetRaw(key,&newDouble,sizeof(double)))<0) {
		return ret;
	}

	key->type=KEY_TYPE_DOUBLE;

	return ret;
}
*/



/**
 * Get the value of a key as a string.
 * If the value can't be represented as a text string (binary value),
 * errno is set to KDB_RET_TYPEMISMATCH.
 *
 * @param returnedString pre-allocated memory to store a copy of the key value
 * @param maxSize number of bytes of pre-allocated memory
 * @return the number of bytes actually copied
 * @see keySetString()
 */
size_t keyGetString(const Key *key, char *returnedString, size_t maxSize) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->data) {
		*returnedString=0;
		errno=KDB_RET_NODATA;
		return 0;
	}

	if (key->dataSize > maxSize) {
		errno=KDB_RET_TRUNC;
		return 0;
	}

	if (key->type < KEY_TYPE_STRING) {
		errno=KDB_RET_TYPEMISMATCH;
		return 0;
	}

	strcpy(returnedString,key->data);
	return key->dataSize;
}





/**
 * Set the value of a key as a string.
 *
 * On disk, text will be encoded to UTF-8.
 *
 * @param newString NULL-terminated text string
 * @return the number of bytes actually copied including final NULL
 * @see keyGetString()
 */
size_t keySetString(Key *key, const char *newString) {
	size_t ret=newString?strblen(newString):0;

	if (!newString || !ret) ret=keySetRaw(key,0,0);
	else keySetRaw(key,newString,ret);

	keySetType(key,KEY_TYPE_STRING);

	return ret;
}




/**
 * Get the value of a binary or string key.
 *
 * @param returnedBinary pre-allocated memory to store a copy of the key value
 * @param maxSize number of bytes of pre-allocated memory
 * @return the number of bytes actually copied
 * @see keySetBinary()
 * @see keyGetString()
 */
size_t keyGetBinary(const Key *key, void *returnedBinary, size_t maxSize) {
	if (!key || !keyIsInitialized(key))
		return errno=KDB_RET_UNINITIALIZED;

	if (!key->data) {
		errno=KDB_RET_NODATA;
		return 0;
	}

	if (key->dataSize > maxSize) {
		errno=KDB_RET_TRUNC;
		return 0;
	}

	memcpy(returnedBinary,key->data,key->dataSize);
	return key->dataSize;
}




/**
 * Set the value of a key as a binary.
 *
 * On disk, value will be encoded into a human readable hex-digit text
 * format and no UTF-8 encoding will be applied.
 *
 * UNIX sysadmins don't like to deal with binary, sand box data.
 * Consider using a string key instead.
 *
 * @param newBinary random bytes
 * @param dataSize number of bytes to copy from newBinary
 * @return the number of bytes actually copied
 * @see keyGetBinary()
 * @see keyGetString()
 * @see keySetString()
 */
size_t keySetBinary(Key *key, const void *newBinary, size_t dataSize) {
	size_t ret=keySetRaw(key,newBinary,dataSize);

	keySetType(key,KEY_TYPE_BINARY);

	return ret;
}




/**
 * Set raw data as the value of a key.
 * If NULL pointers are passed, key value is cleaned.
 * This method will not change or set the key type, and should not be
 * used unless working with user-defined value types.
 *
 * @param newBinary array of bytes to set as the value
 * @param dataSize number bytes to use from newBinary, including the final NULL
 * @see keySetType()
 * @see keySetString()
 * @see keySetBinary()
 */
size_t keySetRaw(Key *key, const void *newBinary, size_t dataSize) {
	if (!key) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}
	if (!keyIsInitialized(key)) keyInit(key);

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

	if (!key->data) return 0;

	memcpy(key->data,newBinary,key->dataSize);
	key->flags |= KEY_SWITCH_VALUE | KEY_SWITCH_NEEDSYNC;
	return key->dataSize;
}






size_t keyGetLink(const Key *key, char *returnedTarget, size_t maxSize) {
/**TODO: Remove or:
 * - update Doc
 * - add keyGetLinkSize()*/
	if (!key || !keyIsInitialized(key))
		return errno=KDB_RET_UNINITIALIZED;

	if (!key->data) {
		errno=KDB_RET_NODATA;
		return 0;
	}

	if (key->type != KEY_TYPE_LINK) {
		errno=KDB_RET_TYPEMISMATCH;
		return 0;
	}

	if (key->dataSize > maxSize) {
		errno=KDB_RET_TRUNC;
		return 0;
	}

	strcpy(returnedTarget,key->data);
	return key->dataSize;
}





size_t keySetLink(Key *key, const char *target) {
	size_t ret=target?strblen(target):0;

	if (!target || !ret) ret=keySetRaw(key,0,0);
	else keySetRaw(key,target,ret);

	keySetType(key,KEY_TYPE_LINK);

	return ret;
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
 * @see keyClose()
 * @see keyInit()
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
 * @see keyGetGID()
 * @see keySetUID()
 * @see keyGetOwner()
 */
uid_t keyGetUID(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

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
 * @see keySetGID()
 * @see keyGetUID()
 * @see keyGetOwner()
 */
int keySetUID(Key *key, uid_t uid) {
	if (!key) return errno=KDB_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key);

	key->uid=uid;
	key->flags |= KEY_SWITCH_UID | KEY_SWITCH_NEEDSYNC;

	return 0;
}



/**
 * Get the system's group ID of a key
 *
 * @return the system's GID of the key
 * @see keySetGID()
 * @see keyGetUID()
 */
gid_t keyGetGID(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

	/*if (!(key->flags & KEY_SWITCH_GID)) return KDB_RET_NOCRED;*/

	return key->gid;
}



/**
 * Set the system's group ID of a key
 *
 * @return the system's GID of the key
 * @see keyGetGID()
 * @see keySetUID()
 */
int keySetGID(Key *key, gid_t gid) {
	if (!key) return errno=KDB_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key);

	key->gid=gid;
	key->flags |= KEY_SWITCH_GID | KEY_SWITCH_NEEDSYNC;

	return 0;
}



/**
 * Return the key filesystem-like access permissions.
 * @see keySetAccess()
 */
mode_t keyGetAccess(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

	/*if (!(key->flags & KEY_SWITCH_PRM)) return KDB_RET_NOCRED;*/

	return key->access;
}




/**
 * Set the key filesystem-like access permissions.
 * @param key the key to set access permissions
 * @param mode the access permissions as for chmod(2)
 * @see keyGetAccess()
 */
int keySetAccess(Key *key, mode_t mode) {
	if (!key) return errno=KDB_RET_UNINITIALIZED;
	if (!keyIsInitialized(key)) keyInit(key);

	key->access=mode;
	key->flags |= KEY_SWITCH_PRM | KEY_SWITCH_NEEDSYNC;

	return 0;
}





/**
 * Get last modification time of the key on disk.
 */
time_t keyGetMTime(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}
	/*if (!(key->flags & KEY_SWITCH_TIME)) return KDB_RET_NOTIME;*/

	return key->mtime;
}



/**
 * Get last time the key data was read from disk.
 */
time_t keyGetATime(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}
	/*if (!(key->flags & KEY_SWITCH_TIME)) return KDB_RET_NOTIME;*/

	return key->atime;
}


/**
 * Get last time the key was stated from disk.
 */
time_t keyGetCTime(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}
	/*if (!(key->flags & KEY_SWITCH_TIME)) return KDB_RET_NOTIME;*/

	return key->ctime;
}



/**
 * Get the number of bytes needed to store this key's parent name.
 * @see keyGetParent()
 */
size_t keyGetParentSize(const Key *key) {
	char *parentNameEnd;

	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (!key->key) {
		errno=KDB_RET_NOKEY;
		return 0;
	}

	/*
		user   (size=0)
		user/parent/base
		user/parent/base/ (size=sizeof("user/parent"))
	*/

	parentNameEnd=rindex(key->key,RG_KEY_DELIM);

	if (!parentNameEnd || parentNameEnd==key->key) {
		/* handle NULL or /something */
		return 0;
	}

	/* handle system/parent/base/ */
	if ((parentNameEnd-key->key) == (strlen(key->key)-1)) {
		parentNameEnd--;
		while (*parentNameEnd!=RG_KEY_DELIM) parentNameEnd--;
	}

	return parentNameEnd - key->key;
}



/**
 * Copy this key's parent name into a pre-allocated buffer.
 *
 * @see keyGetParentSize()
 * @param returnedParent pre-allocated buffer to copy parent name to
 * @param maxSize number of bytes pre-allocated
 */
size_t keyGetParent(const Key *key, char *returnedParent, size_t maxSize) {
	size_t parentSize;

	parentSize=keyGetParentSize(key);

	if (parentSize > maxSize) {
		errno=KDB_RET_TRUNC;
		return 0;
	} else strncpy(returnedParent,key->key,parentSize);

	return parentSize;
}



/**
 * Compare 2 keys.
 *
 * The returned flag array has 1s (different) or 0s (same) for each key meta
 * info compared, that can be logically ORed with @p KEY_SWITCH_* flags.
 *
 * @return a bit array poiting the differences
 * @see ksCompare() for examples and more detailed description
 * @see KeySwitch
 */
u_int32_t keyCompare(const Key *key1, const Key *key2) {
	u_int32_t ret=0;


	/* Compare these numeric properties */
	if (key1->uid != key2->uid)                    ret|=KEY_SWITCH_UID;
	if (key1->gid != key2->gid)                    ret|=KEY_SWITCH_GID;
	if (key1->type != key2->type)                  ret|=KEY_SWITCH_TYPE;
	if ((key1->access & (S_IRWXU|S_IRWXG|S_IRWXO)) !=
		(key2->access & (S_IRWXU|S_IRWXG|S_IRWXO))) ret|=KEY_SWITCH_PRM;

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
	<key id="123445" uid="root" gid="root" mode="0660"
		atime="123456" ctime="123456" mtime="123456"

		name="system/sw/XFree/Monitor/Monitor0/Name"
		type="string">

		<value>Samsung TFT panel</value>
		<comment>My monitor</comment>
	</key>@endverbatim

 * Accepted options that can be ORed:
 * - @p KDB_O_NUMBERS: Do not convert UID and GID into user and group names
 * - @p KDB_O_CONDENSED: Less human readable, more condensed output
 *
 * @param stream where to write output: a file or stdout
 * @param options ORed of KDB_O_* options
 * @see ksToStream()
 * @see KDBOptions
 * @return number of bytes written to output
 */
size_t keyToStream(const Key *key, FILE* stream, unsigned long options) {
	size_t written=0;
	char buffer[800];
	struct passwd *pwd=0;
	struct group *grp=0;


	if (!key || !keyIsInitialized(key) /* || !key->key */) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	if (options & KDB_O_FULLNAME)
		keyGetFullName(key,buffer,sizeof(buffer));
	else keyGetName(key,buffer,sizeof(buffer));

	/* Write key name */
	written+=fprintf(stream,"<key name=\"%s\"", buffer);

	if (options & KDB_O_CONDENSED) written+=fprintf(stream," ");
	else written+=fprintf(stream,"\n     ");



	/* Key type */
	if (options & KDB_O_NUMBERS) {
		written+=fprintf(stream,"type=\"%d\"", keyGetType(key));
	} else {
		u_int8_t type=keyGetType(key);
		buffer[0]=0;

		switch (type) {
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
		else written+=fprintf(stream,"type=\"%d\"", type);
	}


	if (keyIsUser(key)) {
		struct passwd *domainPwd=0;
		int uidMatch,gidMatch;
		
		keyGetOwner(key,buffer,sizeof(buffer));
		domainPwd=getpwnam(buffer);
		pwd=getpwuid(keyGetUID(key));
		grp=getgrgid(keyGetGID(key));
		
		uidMatch=(keyGetUID(key) == domainPwd->pw_uid);
		gidMatch=(keyGetGID(key) == domainPwd->pw_gid);
		
		if (options & KDB_O_FULLUGID) {
			if (pwd && !(options & KDB_O_NUMBERS))
				written+=fprintf(stream," uid=\"%s\"",pwd->pw_name);
			else written+=fprintf(stream,   " uid=\"%d\"",keyGetUID(key));
		
			if (grp && !(options & KDB_O_NUMBERS))
				written+=fprintf(stream," gid=\"%s\"",grp->gr_name);
			else  written+=fprintf(stream,   " gid=\"%d\"",keyGetGID(key));
		} else {
			if (!uidMatch) {
				if (pwd && !(options & KDB_O_NUMBERS))
					written+=fprintf(stream," uid=\"%s\"",pwd->pw_name);
				else written+=fprintf(stream,   " uid=\"%d\"",keyGetUID(key));
			}

			if (!gidMatch) {
				if (grp && !(options & KDB_O_NUMBERS))
					written+=fprintf(stream," gid=\"%s\"",grp->gr_name);
				else written+=fprintf(stream,   " gid=\"%d\"",keyGetGID(key));
			}
		}
	} else {
		if (!(options & KDB_O_NUMBERS)) {
			pwd=getpwuid(keyGetUID(key));
			grp=getgrgid(keyGetGID(key));
		}

		/* UID, GID, mode */
		if (pwd) written+=fprintf(stream," uid=\"%s\"",pwd->pw_name);
		else  written+=fprintf(stream,   " uid=\"%d\"",keyGetUID(key));

		if (grp) written+=fprintf(stream," gid=\"%s\"",grp->gr_name);
		else  written+=fprintf(stream,   " gid=\"%d\"",keyGetGID(key));
	}

	written+=fprintf(stream," mode=\"0%o\">",
		keyGetAccess(key) & (S_IRWXU|S_IRWXG|S_IRWXO));



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




// int keyUnserialize(Key *key,void *buffer) {
// 	void *cursor=buffer;
//
// 	/* A valid serialized key has the following layout
// 	   - Null terminated key name
// 	   - KeyInfo structure
// 	   - Null terminated key group
// 	   - Null terminated key comment
// 	   - Data
// 	*/
//
// 	keySetName(key,cursor);
// 	cursor+=strlen(cursor)+1;
//
// 	return keyUnserializeWithoutName(key,cursor);
// }




// int keyUnserializeWithoutName(Key *key,void *buffer) {
// 	void *cursor=buffer;
//
// 	/* A valid serialized key has the following layout
// 	   - KeyInfo structure
// 	   - Null terminated key group
// 	   - Null terminated key comment
// 	   - Data
// 	*/
//
// 	key->metaInfo=*(KeyInfo *)cursor;
// 	cursor+=sizeof(KeyInfo);
//
// 	keySetGroup(key,cursor);
// 	cursor+=key->groupSize;
//
// 	keySetComment(key,cursor);
// 	cursor+=key->commentSize;
//
// 	keySetRaw(key,cursor,key->dataSize);
//
// 	return KDB_RET_OK;
// }






// int keySerialize(Key *key,void *buffer, size_t maxSize) {
// 	void *cursor=buffer;
// 	size_t keySize;
//
// 	if (!key) return KDB_RET_NULLKEY;
// 	if (!keyIsInitialized(key)) return KDB_RET_UNINITIALIZED;
// 	if (!(key->flags & KEY_SWITCH_NAME)) return KDB_RET_NOKEY;
//
// 	/* A valid serialized key has the following layout
// 	   - Null terminated full key name
// 	   - KeyInfo structure
// 	   - Null terminated key group
// 	   - Null terminated key comment
// 	   - Data
// 	*/
//
// 	/* The key name */
// 	keyGetFullName(key,cursor,maxSize);
// 	keySize=keyGetFullNameSize(key);
// 	cursor+=keySize+1;
//
// 	return keySerializeWithoutName(key,cursor,maxSize-(cursor-buffer));
// }








// int keySerializeWithoutName(Key *key,void *buffer, size_t maxSize) {
// 	void *cursor=buffer;
//
// 	if (!key) return KDB_RET_NULLKEY;
// 	if (!keyIsInitialized(key)) return KDB_RET_UNINITIALIZED;
// 	if (!(key->flags & KEY_SWITCH_NAME)) return KDB_RET_NOKEY;
//
// 	/* A valid serialized key has the following layout
// 	   - Null terminated key name
// 	   - KeyInfo structure
// 	   - Null terminated key group
// 	   - Null terminated key comment
// 	   - Data
// 	*/
//
// 	/* The KeyInfo whole struct */
// 	memcpy(cursor, &key->metaInfo, sizeof(KeyInfo));
// 	cursor+=sizeof(KeyInfo);
//
// 	/* The group name */
// 	if (key->group)
// 		memcpy(cursor, key->group, key->groupSize);
// 	else *(char *)cursor=0;
// 	if (key->groupSize<=1) cursor++;
// 	else cursor+=key->groupSize;
//
//
// 	/* The description */
// 	if (key->comment) memcpy(cursor, key->comment, key->commentSize);
// 	else *(char *)cursor=0;
// 	if (key->commentSize<=1) cursor++;
// 	else cursor+=key->commentSize;
//
//
// 	/* The data */
// 	if (key->data)
// 		memcpy(cursor,key->data,key->dataSize);
//
// 	return KDB_RET_OK;
// }



/**
 * Check whether a key name is under the @p system namespace or not
 *
 * @return 1 if string begins with @p system , 0 otherwise
 * @param keyName the name of a key
 * @see keyIsSystem()
 * @see keyIsUser()
 * @see keyNameIsUser()
 *
 */
int keyNameIsSystem(const char *keyName) {
	if (!keyName) return 0;
	if (!strlen(keyName)) return 0;

	if (!strncmp("system",keyName,sizeof("system")-1)) return 1;
	return 0;
}


/**
 * Check whether a key name is under the @p user namespace or not
 *
 * @return 1 if string begins with @p user, 0 otherwise
 * @param keyName the name of a key
 * @see keyIsSystem()
 * @see keyIsUser()
 * @see keyNameIsSystem()
 *
 */
int keyNameIsUser(const char *keyName) {
	if (!keyName) return 0;
	if (!strlen(keyName)) return 0;

	if (!strncmp("user",keyName,sizeof("user")-1)) return 1;
	return 0;
}



/**
 * Check whether a key is under the @p system namespace or not
 *
 * @return 1 if key name begins with @p system, 0 otherwise
 * @see keyNameIsSystem()
 * @see keyIsUser()
 * @see keyNameIsUser()
 *
 */
int keyIsSystem(const Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return keyNameIsSystem(key->key);
}



/**
 * Check whether a key is under the @p user namespace or not
 *
 * @return 1 if key name begins with @p user, 0 otherwise
 * @see keyNameIsSystem()
 * @see keyIsSystem()
 * @see keyNameIsUser()
 *
 */
int keyIsUser(const Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return keyNameIsUser(key->key);
}




/**
 * Return the namespace of a key name
 *
 * Currently valid namespaces are KEY_NS_SYSTEM and KEY_NS_USER.
 *
 * @return KEY_NS_SYSTEM, KEY_NS_USER or 0
 * @see keyGetNamespace()
 * @see keyIsUser()
 * @see keyIsSystem()
 * @see KeyNamespace
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
 * Currently valid namespaces are KEY_NS_SYSTEM and KEY_NS_USER.
 *
 * @return KEY_NS_SYSTEM, KEY_NS_USER or 0
 * @see keyNameGetNameSpace()
 * @see keyIsUser()
 * @see keyIsSystem()
 * @see KeyNamespace
 *
 */
int keyGetNamespace(const Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return keyNameGetNamespace(key->key);
}


/**
 * Check if a key is folder key
 *
 * Folder keys have no value.
 *
 * @return 1 if key is a folder, 0 otherwise
 * @see keyIsLink()
 * @see keyGetType()
 */
int keyIsDir(const Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return (S_ISDIR(key->access) || (key->type==KEY_TYPE_DIR));
}


/**
 * Check if a key is a link key
 *
 * The value of link keys is the key they point to.
 *
 * @return 1 if key is a link, 0 otherwise
 * @see keyIsDir()
 * @see keyGetType()
 */
int keyIsLink(const Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;

	return (S_ISLNK(key->access) || (key->type==KEY_TYPE_LINK));
}


/**
 * Gets number of bytes needed to store root name of a key name
 *
 * Possible root key names are @p system, @p user or @p "user:someuser" .
 *
 * @return number of bytes needed without ending NULL
 * @param keyName the name of the key
 * @see keyGetRootNameSize()
 */
size_t keyNameGetRootNameSize(const char *keyName) {
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
 * @see keyGetFullRootNameSize()
 * @see keyNameGetRootNameSize()
 */
size_t keyGetRootNameSize(const Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;
	if (!key->key) return 0;

	return keyNameGetRootNameSize(key->key);
}





/**
 * Calculates number of bytes needed to store full root name of a key.
 *
 * Possible root key names are @p system, @p user or @p user:someuser.
 * In contrast to keyGetRootNameSize(), this method considers the user
 * domain part, and you should prefer this one.
 *
 * @return number of bytes needed without ending NULL
 * @see keyNameGetRootNameSize()
 * @see keyGetRootNameSize()
 */
size_t keyGetFullRootNameSize(const Key *key) {
	size_t size=0;

	if (keyIsUser(key)) {
		if (key->userDomain) size=strblen(key->userDomain);
		else size=strblen(getenv("USER"));
	}

	return size+keyNameGetRootNameSize(key->key);
}



/**
 * Calculates number of bytes needed to store basename of a key name.
 *
 * Basenames are denoted as:
 * - @p system/some/thing/basename
 * - @p user:domain/some/thing/basename
 *
 * @return number of bytes needed without ending NULL
 * @see keyGetBaseNameSize()
 */
size_t keyNameGetBaseNameSize(const char *keyName) {
	char *end;
	size_t size,keySize;
	unsigned char found=0;

	if (!(keySize=strblen(keyName))) return 0;

	size=keyNameGetRootNameSize(keyName);
	if (!size || size==keySize) return 0; /* key is a root key */

	/* Possible situations left:

		system/something/basename
		system/something/basename/
	*/

	end=strrchr(keyName,RG_KEY_DELIM);
	if (*(end-1)!='\\') return keyName+keySize-(end+1);

	/* TODO: review bellow this point. obsolete code */
	/* Possible situations left:

		system/something/base\.name
		system/something/basename\.
	*/

	while (!found) {
		end--;
		if (*end=='.') found=1;
	}
	return keyName+keySize-(end+1);
}




/**
 * Calculates number of bytes needed to store basename of a key.
 *
 * Basenames are denoted as:
 * - @p system/some/thing/basename
 * - @p user:domain/some/thing/basename
 *
 * @return number of bytes needed without ending NULL
 * @see keyNameGetBaseNameSize()
 */
size_t keyGetBaseNameSize(const Key *key) {
	if (!key) return 0;
	if (!keyIsInitialized(key)) return 0;
	if (!key->key) return 0;

	return keyNameGetBaseNameSize(key->key);
}




/**
 * Copy to @p returned the root name of the key.
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
 * @see keyNameGetRootNameSize()
 * @see keyGetRootNameSize()
 * @see keyGetFullRootName()
 */
size_t keyGetRootName(const Key *key, char *returned, size_t maxSize) {
	size_t size;

	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

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
 * @see keyGetFullRootNameSize()
 * @see keyGetRootName()
 */
size_t keyGetFullRootName(const Key *key, char *returned, size_t maxSize) {
	size_t size;
	size_t userSize;
	char *cursor;

	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

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
		return 0;
	}
	
	userSize = keyGetRootNameSize (key);
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
 * @see keyGetBaseNameSize()
 */
size_t keyGetBaseName(const Key *key, char *returned, size_t maxSize) {
	size_t size;
	size_t keySize;

	if (!key) {
		errno=KDB_RET_NULLKEY;
		return 0;
	}

	if (!keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

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
 * Set a general flag in the Key
 *
 * The flag has no semantics to the library, only to your application.
 *
 * @see keyGetFlag()
 * @see keyClearFlag()
 * @return 0, unless key is invalid.
 */
int keySetFlag(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

	key->flags|=KEY_SWITCH_FLAG;

	return 0;
}



/**
 * Clear the general flag in the Key
 *
 * The flag has no semantics to the library, only to your application.
 *
 * @see keyGetFlag()
 * @see keySetFlag()
 * @return 0, unless key is invalid.
 */
int keyClearFlag(Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return -1;
	}

	key->flags &= ~KEY_SWITCH_FLAG;

	return 0;
}






/**
 * Get the flag from the Key.
 *
 * The flag has no semantics to the library, only to your application.
 *
 * @see keySetFlag()
 * @see keyClearFlag()
 * @see keyNew() with KEY_SWITCH_NEEDSYNC
 * @return 1 if flag is set, 0 otherwise
 */
int keyGetFlag(const Key *key) {
	if (!key || !keyIsInitialized(key)) {
		errno=KDB_RET_UNINITIALIZED;
		return 0;
	}

	return (key->flags | KEY_SWITCH_FLAG) ? 1:0;
}

/**
 * @} // end of Key group
 */



/**
 * @defgroup keyset KeySet Class Methods
 * @brief Methods to manipulate KeySets.
 * A KeySet is a linked list to group a number of Keys.
 * Key Sets have an internal cursor to help in the Key navigation.
 *
 * These are the methods to make various manipulations in the objects of class KeySet.
 * Methods for sorting, merging, comparing, and internal cursor manipulation are provided.
 * To use it:
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
 * allocated by ksNew())
 *
 * @see ksNew()
 * @see ksClose()
 * @return whatever is returned by ksClose()
 */
int ksDel(KeySet *ks) {
	int rc;
	
	rc=ksClose(ks);
	free(ks);
	
	return rc;
}



/**
 * Same as ksDel(), provided as different function name option.
 */
int ksFree(KeySet *ks) {
	return ksDel(ks);
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



/**
 * KeySet object initializer.
 * 
 * You should always use ksNew() instead of ksInit().
 *
 * Every KeySet object that will be used must be initialized first, to setup
 * pointers, counters, etc. After use, all ksInit()ialized KeySets must be
 * cleaned with ksClose().
 * 
 * @see ksNew()
 * @see ksClose()
 * @see keyInit()
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
 * @see keyDel()
 * @see ksInit()
 * @see keyClose()
 * @see ksAppend() for details on how keys are inserted in KeySets
 * @return 0
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
 * @return the number of keys contained by the @p ks.
 *
 */
size_t ksGetSize(KeySet *ks) {
	return ks->size;
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
 * @see ksRewind()
 * @see ksCurrent()
 * @return the new current Key
 *
 */
Key *ksNext(KeySet *ks) {
	if (ks->cursor) ks->cursor=ks->cursor->next;
	else ks->cursor=ks->start;

	return ks->cursor;
}



/**
 * Resets a KeySet internal cursor.
 * Use it to set the cursor to the begining of the KeySet
 *
 * @see ksNext()
 * @see ksCurrent()
 * @see kdbMonitorKeys() for an example
 * @return allways 0
 *
 */
int ksRewind(KeySet *ks) {
	ks->cursor=0;
	return 0;
}



/**
 * Return the current Key
 *
 * @see ksNext()
 * @see ksRewind()
 * @see kdbMonitorKeys() for a usage example
 * @return pointer to the Key pointed by @p ks's cursor
 */
Key *ksCurrent(const KeySet *ks) {
	return ks->cursor;
}




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
 * @see ksAppend()
 * @see ksInsertKeys()
 * @see ksAppendKeys()
 * @see ksDel()
 * @see keyNew()
 *
 */
size_t ksInsert(KeySet *ks, Key *toInsert) {
	toInsert->next=ks->start;
	ks->start=toInsert;
	if (!ks->end) ks->end=toInsert;
	return ++ks->size;
}




/**
 * Transfers all keys from @p toInsert to the begining of @p ksa.
 *
 * After this call, @p toInsert will be empty.
 *
 * @return the size of the KeySet after insertion
 * @param ks the KeySet that will receive the keys
 * @param toInsert the KeySet that provides the keys that will be transfered
 * @see ksAppend()
 * @see ksInsert()
 * @see ksAppendKeys()
 *
 */
size_t ksInsertKeys(KeySet *ks, KeySet *toInsert) {
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
 * Appends a new Key to the end @p ks. A reference to the key will
 * be stored, and not a private copy. So a future ksClose() or ksDel() on
 * @p ks will keyDel() the @p toAppend object.
 * The KeySet internal cursor is not moved.
 *
 * Do not ksAppend() Keys that are already contained by other KeySets.
 *
 * @return the size of the KeySet after insertion
 * @param ks KeySet that will receive the key
 * @param toAppend Key that will be appended to ks
 * @see ksDel()
 * @see ksClose()
 * @see ksInsert()
 * @see ksInsertKeys()
 * @see ksAppendKeys()
 * @see keyNew()
 *
 */
size_t ksAppend(KeySet *ks, Key *toAppend) {
	toAppend->next=0;
	if (ks->end) ks->end->next=toAppend;
	if (!ks->start) ks->start=toAppend;
	ks->end=toAppend;
	return ++ks->size;
}



/**
 * Transfers all @p toAppend contained keys to the end of the @p ks.
 *
 * After this call, the @p toAppend KeySet will be empty.
 *
 * @return the size of the KeySet after transfer
 * @param ks the KeySet that will receive the keys
 * @param toAppend the KeySet that provides the keys that will be transfered
 * @see ksAppend()
 * @see ksInsert()
 * @see ksInsertKeys()
 * 
 */
size_t ksAppendKeys(KeySet *ks, KeySet *toAppend) {
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







/**
 *  Compare 2 KeySets with the following behavior:
 *  - A key (by full name) that is present on @p ks1 and @p ks2, and has
 *    something different, will be transfered from @p ks2 to @p ks1, and
 *    @p ks1's (old) version deleted.
 *  - Keys that are in @p ks1, but aren't in @p ks2 will be trasnsfered
 *    from @p ks1 to @p removed.
 *  - Keys that are keyCompare() equal in @p ks1 and @p ks2 will be
 *    deleted from @p ks2.
 *  - Keys that are available in @p ks2 but don't exist in @p ks1 will
 *    be transfered to @p ks1.
 *
 *  In the end, @p ks1 will have all the keys that matter, and @p ks2
 *  will be empty.
 *
 *  After ksCompare(), you should:
 *  - ksDel(ks2)
 *  - call kdbSetKeys() on @p ks1 to commit all changed keys
 *  - kdbRemove() for all keys in the @p removed KeySet
 *
 *  @see keyCompare()
 *  @see commandEdit() at the kdb command
 *  @param ks1 first KeySet
 *  @param ks2 second KeySet
 *  @param removed initially empty KeySet that will be filled with keys
 *  removed from @p ks1
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
 * Prints an XML version of a KeySet object.
 *
 * Accepted options:
 * - @p KDB_O_NUMBERS: Do not convert UID and GID into user and group names
 * - @p KDB_O_CONDENSED: Less human readable, more condensed output
 * - @p KDB_O_XMLHEADERS: Include the correct XML headers in the output. Use it.
 *
 * @param stream where to write output: a file or stdout
 * @param options ORed of @p KDB_O_* options
 * @see keyToStream()
 * @see KDBOptions
 * @return number of bytes written to output
 */
size_t ksToStream(const KeySet *ks, FILE* stream, unsigned long options) {
	size_t written=0;
	Key *key=0;

	if (options & KDB_O_XMLHEADERS) {
		written+=fprintf(stream,"<?xml version=\"1.0\" encoding=\"%s\"?>\n",
			nl_langinfo(CODESET));
		written+=fprintf(stream,
			"<!DOCTYPE keyset PUBLIC \"-//Avi Alkalay//DTD Elektra 0.1.0//EN\" \"http://elektra.sf.net/dtd/elektra.dtd\">\n\n\n");
		written+=fprintf(stream,
			"<!-- Generated by Elektra API. Total of %d keys. -->\n\n\n\n",ks->size);
	}

	written+=fprintf(stream,"<keyset>\n\n\n");
	
	for (key=ks->start; key; key=key->next)
		written+=keyToStream(key,stream,options);
	
	written+=fprintf(stream,"</keyset>\n");
	return written;
}



/* Used by the qsort() function */
int keyCompareByName(const void *p1, const void *p2) {
	Key *key1=*(Key **)p1;
	Key *key2=*(Key **)p2;

	return strcmp(key1->key, key2->key);
}


/**
 * Sorts a KeySet aphabetically by Key names.
 *
 * @param ks KeySet to be sorted
 */
void ksSort(KeySet *ks) {
	Key *keys[ks->size];
	Key *cursor;
	size_t c=0;

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



/*
 * KDB_O_SORT: the ks is NOT sorted
 * KDB_O_NOSPANPARENT: find under current subtree only
 * KDB_O_CYCLE: restart from begining of KeySet if end reached
 * 
 *
Key *ksFindRE(const KeySet *ks, const regex_t *regexp, unsigned long options) {
	Key *current, *walker;
	regmatch_t matched;
	u_int rc;
	
	current=ks->cursor;
	
	// TODO: finish this method
	
	rc=regexec(regexp,walker->key,1,&matched,0);
	
	if (rc == 0) return current;
	else return 0;
}
*/
/**
 * @} // end of KeySet group
 */

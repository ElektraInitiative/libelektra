/***************************************************************************
                kdb.h  -  Exported methods of the Elektra Project
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

#ifndef KDB_H
#define KDB_H



/*
 * @defgroup general Elektra General definitions
 * @brief Some global definitions when using the Elektra API
 *
 */



#include <sys/types.h>
#include <errno.h>
#include <stdio.h>

#define RG_KEY_DELIM            '/'


/* When FORMAT_VERSION changes, FORMAT must change also. */
#define RG_KEY_FORMAT_VERSION   2
#define RG_KEY_FORMAT           "RG002"


/**
 * Key data types.
 *
 * Key type values grow from the semantically poor to the semantically rich.
 * The gaps between them is for user-defined types.
 *
 * If your application needs value types with more semantics, like @p Color,
 * @p Font, etc, you can still use it. You'll have to define a new type
 * number in the scope of your application, and force the type with
 * keySetType().
 *
 * The type number is a value between 0 and 255. If you define it
 * bigger than @p KEY_TYPE_STRING, it will be still treated as a string
 * (in the terms of Unicode handling). If you define it between
 * @p KEY_TYPE_BINARY and @p KEY_TYPE_STRING, Elektra will handle it as
 * a binary, will not make Unicode handling and will save it hex-encoded.
 *
 * @ingroup key
 * @see keyGetType()
 * @see keySetType() for an example of how to define custom types
 */
enum KeyType {
	KEY_TYPE_UNDEFINED=0, /*!< Undefined key type */
	KEY_TYPE_DIR=1, /*!< A directory key */
	KEY_TYPE_LINK=2, /*!< A symbolink link key.
	                      This gap is for special key meta types,
	                      that can't go into regular files. */
	KEY_TYPE_BINARY=20, /*!< A binary key.
	                         This gap is for binary data types
	                         that have some semantics that somebody
	                         can invent in the future */
	KEY_TYPE_STRING=40 /*!< A string key */
};






/**
 * Elektra currently supported Key namespaces.
 * @ingroup key
 * @see kdbGetRootKeys()
 * @see keyGetNamespace()
 * @see keyNameGetNamespace()
 */
enum KeyNamespace {
	KEY_NS_SYSTEM=1,  /*!< The @p system keys */
	KEY_NS_USER=2  /*!< The @p user keys */
};



/*
The key flags bit array. The '.' means whatever:

7654 3210 7654 3210 7654 3210 7654 3210
...1 0... .0.. .1.. ..1. ..0. ...0 1... 0x10042008 Initialized
...1 1... .1.. .1.. ..1. ..1. ...1 1... 0x18442218 Initialized mask
.... .... .... .... .... .... .... ...1 0x00000001 HASTYPE
.... .... .... .... .... .... .... ..1. 0x00000002 HASKEY
.... .... .... .... .... .... .... .1.. 0x00000004 HASDATA
.... .... .... .... .... .... ..1. .... 0x00000020 HASDOMAIN
.... .... .... .... .... .... .1.. .... 0x00000040 HASCOMMENT
.... .... .... .... .... .... 1... .... 0x00000060 HASUID
.... .... .... .... .... ...1 .... .... 0x00000080 HASGID
.... .... .... .... .... .1.. .... .... 0x00000400 HASPRM
.... .... .... .... .... 1... .... .... 0x00000800 HASTIME
.... .... .... .... ...1 .... .... .... 0x00001000 NEEDSYNC
.... .... .... .... .1.. .... .... .... 0x00004000 ACTIVE ***DEPRECATED***
1... .... .... .... .... .... .... .... 0x80000000 FLAG (general flag)
*/



/**
 * Flags that can be used with keys.
 * @ingroup key
 * @see keyCompare()
 * @see kdbMonitorKey() the diffMask parameter
 * @see kdbMonitorKeys() the diffMask parameter
 * @see keyGetFlag()
 */
enum KeyFlags {
	KEY_FLAG_INITIALIZED=0x10042008,
	KEY_FLAG_INITMASK=0x18442218,

	KEY_FLAG_HASTYPE=1,       /*!< Flag for the key type */
	KEY_FLAG_HASKEY=1<<1,     /*!< Flag for the key name */
	KEY_FLAG_HASDATA=1<<2,    /*!< Flag for the key data */
	KEY_FLAG_HASDOMAIN=1<<5,  /*!< Flag for the key user domain */
	KEY_FLAG_HASCOMMENT=1<<6, /*!< Flag for the key comment */
	KEY_FLAG_HASUID=1<<7,     /*!< Flag for the key UID */
	KEY_FLAG_HASGID=1<<8,     /*!< Flag for the key GID */
	KEY_FLAG_HASPRM=1<<10,    /*!< Flag for the key permissions */
	KEY_FLAG_HASTIME=1<<11,   /*!< Flag for the key change time */
	KEY_FLAG_NEEDSYNC=1<<12,  /*!< Flags that key needs syncronization */
	KEY_FLAG_ACTIVE=1<<14,  /* ****deprecated**** */
	KEY_FLAG_FLAG=1<<31,      /*!< General purpose flag that has semantics
	                               only to your app */
};



/**
 * Some return codes generated by the Elektra library.
 *
 * These are only specific Elektra errors, that the library sets in errno.
 * Other error can be generated by system calls that the API uses.
 * Then errno is propagated.
 *
 * A very robust program should check errno after each API call.
 *
 * @ingroup kdb
 */
enum KDBErr {
	KDB_RET_OK=0,                 /*!< No error */
	KDB_RET_NULLKEY=EINVAL,       /*!< Invalid Key object */
	KDB_RET_UNINITIALIZED=EINVAL, /*!< Object not initilized */
	KDB_RET_NOKEY=ENOMSG,         /*!< Key has no name */
	KDB_RET_NODATA=ENOMSG,        /*!< Key has no data */
	KDB_RET_NOGROUP=ENOMSG,       /*!< Key has no group */
	KDB_RET_NODESC=ENOMSG,        /*!< Key has no comment/description */
	KDB_RET_NODOMAIN=ENOMSG,      /*!< Key has no user domain set */
	KDB_RET_NOCRED=EACCES,        /*!< No credentials to access resource */
	KDB_RET_NOTIME=ENOMSG,        /*!< Key has no access time set */
	KDB_RET_TRUNC=ENOBUFS,        /*!< Buffer was too small */
	KDB_RET_TYPEMISMATCH=EILSEQ,  /*!< Failed to convert key data due to data type */
	KDB_RET_INVALIDKEY=EAFNOSUPPORT, /*!< Key name is no 'system/' or 'user/' */
	KDB_RET_NOTFOUND=ENOENT,      /*!< Key was not found */
};


/**
 * Options to change the default behavior of some methods.
 *
 * These options should be ORed.
 * @ingroup kdb
 * @see kdbGetChildKeys()
 * @see ksToStream()
 * @see keyToStream()
 */
enum KDBOptions {
	KDB_O_RECURSIVE=1,      /*!< Act recursively */
	KDB_O_DIR=1<<1,         /*!< Include dir keys in result */
	KDB_O_NOVALUE=1<<2,     /*!< Retrieve only keys that don't have values (a.k.a dir keys) */
	KDB_O_NOEMPTY=1<<3,     /* unused ???? */
	KDB_O_STATONLY=1<<4,    /*!< Only stat key, instead of getting entirelly */
	KDB_O_INACTIVE=1<<5,    /*!< Do not ignore inactive keys (that name begins with .) */
	KDB_O_SORT=1<<6,        /*!< Sort keys */
	KDB_O_NFOLLOWLINK=1<<7, /*!< Do not follow symlinks */

/* XML exporting options for keytoStrem() */
	KDB_O_CONDENSED=1<<8,   /*!< Compressed XML, not usefull for human eyes */
	KDB_O_NUMBERS=1<<9,     /*!< Use UID and GID intead of user and group names */
	KDB_O_XMLHEADERS=1<<10, /*!< Show also the XML header of the document */
} kdbOption;


typedef struct _Key {
	 u_int8_t      type;        /* data type */
	    uid_t      uid;         /* owner user ID */
	    uid_t      gid;         /* owner group ID */
	   mode_t      access;      /* access control */
	   time_t      atime;       /* time for last access */
	   time_t      mtime;       /* time for last modidification */
	   time_t      ctime;       /* time for last change (meta info) */
	   size_t      commentSize; /* size of the description string */
	   size_t      dataSize;    /* size of the value */
	   size_t      recordSize;  /* dataSize + commentSize + control */
	u_int32_t      flags;       /* Some control flags */
	   char *      key;         /* The name of the key */
	   char *      comment;     /* A comment about this key-value pair */
	   char *      userDomain;  /* User domain */
	   void *      data;        /* The value */
	struct _Key *  next;
} Key;



/* Key Name Anatomy

Key::key is the key name. It is a unique identifier for a kdb key.
An exmaple of a complex key name is:

	user:some.user/My Environment/SOME.VAR

From this example:
  Root name = "user:some.user"
      Owner = "some.user"
  Base name = "SOME.VAR"
Parent name = "user:some.user/My Environment"

*/





typedef struct _KeySet {
	Key *          start;
	Key *          end;
	Key *          cursor;
	size_t         size;
} KeySet;





/**************************************

KeyDB methods

***************************************/

int kdbOpen();
int kdbClose();

int kdbGetValue(const char *keyname, char *returned,size_t maxSize);
int kdbGetKeyByParent(const char *parentName, const char *baseName, Key *returned);
int kdbGetKeyByParentKey(const Key *parent, const char *baseName, Key *returned);
int kdbGetValueByParent(const char *parentName, const char *baseName, char *returned,
	size_t maxSize);

int kdbSetValue(const char *keyname, const char *value);
int kdbSetValueByParent(const char *parentName, const char *baseName, const char *value);

int kdbRemove(const char *keyName);
int kdbLink(const char *oldPath, const char *newKeyName);

int kdbGetKeyByParent(const char *parentName, const char *baseName, Key *returned);
int kdbGetKeyByParentKey(const Key *parent, const char *basename, Key *returned);
int kdbGetValueByParent(const char *parentName, const char *baseName, char *returned,
	size_t maxSize);

int kdbGetComment(const char *keyname, char *returned, size_t maxSize);
size_t kdbSetComment(const char *keyname, const char *comment);

int kdbStatKey(Key *key);
int kdbGetKey(Key *key);
int kdbSetKey(Key *key);

int kdbGetChildKeys(const char *parent, KeySet *returned, unsigned long options);
int kdbGetRootKeys(KeySet *returned);

int kdbSetKeys(KeySet *ks);

u_int32_t kdbMonitorKey(Key *interest, u_int32_t diffMask,
	unsigned long iterations, unsigned usleep);
u_int32_t kdbMonitorKeys(KeySet *interests, u_int32_t diffMask,
	unsigned long iterations, unsigned sleep);






/**************************************

Key methods

***************************************/

int keyInit(Key *key);
int keyClose(Key *key);

int keyIsInitialized(const Key *key);
int keyNeedsSync(const Key *key);
int keyDup(const Key *source,Key *dest);

int keySerialize(const Key *key,void *buffer, size_t maxSize);
int keyUnserialize(Key *key,const void *buffer);
size_t keyGetSerializedSize(const Key *key);

u_int8_t keyGetType(const Key *key);
u_int8_t keySetType(Key *key,u_int8_t type);

int keySetFlag(Key *key);
int keyClearFlag(Key *key);
int keyGetFlag(const Key *key);

size_t keyGetRecordSize(const Key *key);
size_t keyGetNameSize(const Key *key);
size_t keyGetFullNameSize(const Key *key);

size_t keyGetName(const Key *key, char *returnedName, size_t maxSize);
size_t keySetName(Key *key, const char *newName);

size_t keyGetFullName(const Key *key, char *returnedName, size_t maxSize);
size_t keyGetRootName(const Key *key, char *returned, size_t maxSize);
size_t keyGetFullRootName(const Key *key, char *returned, size_t maxSize);

size_t keyGetBaseName(const Key *key, char *returned, size_t maxSize);
size_t keyNameGetBaseNameSize(const char *keyName);
size_t keyGetBaseNameSize(const Key *key);

size_t keyNameGetRootNameSize(const char *keyName);
size_t keyGetRootNameSize(const Key *key);
size_t keyGetFullRootNameSize(const Key *key);


size_t keyGetCommentSize(const Key *key);
size_t keyGetComment(const Key *key, char *returnedDesc, size_t maxSize);
size_t keySetComment(Key *key, const char *newDesc);

uid_t keyGetUID(const Key *key);
int keySetUID(Key *key, uid_t uid);

gid_t keyGetGID(const Key *key);
int keySetGID(Key *key, gid_t gid);

mode_t keyGetAccess(const Key *key);
int keySetAccess(Key *key, mode_t mode);

size_t keyGetOwner(const Key *key, char *returned, size_t maxSize);
size_t keySetOwner(Key *key, const char *userDomain);


size_t keyGetDataSize(const Key *key);

size_t keyGetString(const Key *key, char *returnedString, size_t maxSize);
size_t keySetString(Key *key, const char *newString);

size_t keyGetBinary(const Key *key, void *returnedBinary, size_t maxSize);
size_t keySetBinary(Key *key, const void *newBinary, size_t dataSize);

size_t keyGetLink(const Key *key, char *returnedTarget, size_t maxSize);
size_t keySetLink(Key *key, const char *target);

time_t keyGetMTime(const Key *key);
time_t keyGetATime(const Key *key);
time_t keyGetCTime(const Key *key);

size_t keyGetParentName(const Key *key, char *returned, size_t maxSize);

size_t keyToString(const Key *key, char *returned, size_t maxSize);

int keyIsSystem(const Key *key);
int keyNameIsSystem(const char *keyName);

int keyIsUser(const Key *key);
int keyNameIsUser(const char *keyName);

int keyGetNamespace(const Key *key);
int keyNameGetNamespace(const char *keyName);

int keyIsDir(const Key *key);
int keyIsLink(const Key *key);

u_int32_t keyCompare(const Key *key1, const Key *key2);

size_t keyToStream(const Key *key, FILE* stream, unsigned long options);


/**************************************

KeySet methods

***************************************/

int ksInit(KeySet *ks);
int ksClose(KeySet *ks);

size_t ksInsert(KeySet *ks, Key *toInsert);
size_t ksAppend(KeySet *ks, Key *toAppend);

size_t ksInsertKeys(KeySet *ks, KeySet *toInsert);
size_t ksAppendKeys(KeySet *ks, KeySet *toAppend);

size_t ksToStream(const KeySet *ks, FILE* stream, unsigned long options);
int ksCompare(KeySet *ks1, KeySet *ks2, KeySet *removed);

int ksRewind(KeySet *ks);
Key *ksNext(KeySet *ks);
Key *ksCurrent(const KeySet *ks);


// Key *ksLookupByName(KeySet *ks,char *keyName);
// Key *ksLookupByRegex(KeySet *ks,regex_t *regex);




/***************************************

Helpers

***************************************/


size_t strblen(const char *s);


#endif /* KDB\_H */

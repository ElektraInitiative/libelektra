/***************************************************************************
                kdb.h  -  Exported methods of the Elektra Library
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
#include <regex.h> 

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
 * keySetType(), or keyNew().
 *
 * The type number is a value between 0 and 255. If your user-defined
 * type >= @p KEY_TYPE_STRING, it will be still treated as a string
 * (in the terms of Unicode handling). If 
 * @p KEY_TYPE_BINARY <= type < @p KEY_TYPE_STRING, Elektra will handle it
 * as a binary value, will not make Unicode handling and will save it
 * hex-encoded.
 *
 * @ingroup key
 * @see keyGetType()
 * @see keySetType() for an example of how to define custom types
 */
enum KeyType {
	KEY_TYPE_UNDEFINED=0,  /*!< Undefined key type */
	KEY_TYPE_DIR=1,        /*!< A directory key */
	KEY_TYPE_LINK=2,       /*!< A symbolink link key.
	                          This gap is for special key meta types,
	                          that can't go into regular files. */
	KEY_TYPE_BINARY=20,    /*!< A binary key.
	                          This gap is for binary data types
	                          that have some semantics that somebody
	                          can invent in the future */
	KEY_TYPE_STRING=40     /*!< A string key */
};






/**
 * Elektra currently supported Key namespaces.
 * @ingroup key
 * @see kdbGetRootKeys()
 * @see keyGetNamespace()
 * @see keyNameGetNamespace()
 */
enum KeyNamespace {
	KEY_NS_SYSTEM=1,       /*!< The @p system keys */
	KEY_NS_USER=2          /*!< The @p user keys */
};



/*
The key flags bit array. The '.' means whatever:

7654 3210 7654 3210 7654 3210 7654 3210
...1 0... .0.. .1.. ..1. ..0. ...0 1... 0x10042008 Initialized
...1 1... .1.. .1.. ..1. ..1. ...1 1... 0x18442218 Initialized mask
.... .... .... .... .... .... .... ...1 0x00000001 TYPE
.... .... .... .... .... .... .... ..1. 0x00000002 NAME
.... .... .... .... .... .... .... .1.. 0x00000004 VALUE
.... .... .... .... .... .... ..1. .... 0x00000020 DOMAIN
.... .... .... .... .... .... .1.. .... 0x00000040 COMMENT
.... .... .... .... .... .... 1... .... 0x00000060 UID
.... .... .... .... .... ...1 .... .... 0x00000080 GID
.... .... .... .... .... .1.. .... .... 0x00000400 PRM
.... .... .... .... .... 1... .... .... 0x00000800 TIME
.... .... .... .... ...1 .... .... .... 0x00001000 NEEDSYNC
.... .... .... .... .1.. .... .... .... 0x00004000 ACTIVE ***DEPRECATED***
1... .... .... .... .... .... .... .... 0x80000000 FLAG (general flag)
*/



/**
 * Switches to denote the various Key attributes in methods throughout
 * this library.
 * @ingroup key
 * @see keyNew()
 * @see keyCompare()
 * @see kdbMonitorKey(), kdbMonitorKeys(), the diffMask parameter
 * @see keyGetFlag(), keySetFlag()
 */
enum KeySwitch {
	KEY_SWITCH_TYPE=1,          /*!< Flag for the key type */
	KEY_SWITCH_NAME=1<<1,       /*!< Flag for the key name */
	KEY_SWITCH_VALUE=1<<2,      /*!< Flag for the key data */
	KEY_SWITCH_OWNER=1<<5,      /*!< Flag for the key user domain */
	KEY_SWITCH_DOMAIN=KEY_SWITCH_OWNER, /*!< An alias */
	KEY_SWITCH_COMMENT=1<<6,    /*!< Flag for the key comment */
	KEY_SWITCH_UID=1<<7,        /*!< Flag for the key UID */
	KEY_SWITCH_GID=1<<8,        /*!< Flag for the key GID */
	KEY_SWITCH_MODE=1<<10,      /*!< Flag for the key permissions */
	KEY_SWITCH_TIME=1<<11,      /*!< Flag for the key change time */
	KEY_SWITCH_NEEDSYNC=1<<12,  /*!< Flags that key needs syncronization */
	KEY_SWITCH_ACTIVE=1<<14,    /* ****deprecated**** */
	KEY_SWITCH_FLAG=1<<31,      /*!< General purpose flag that has semantics
	                                 only to your app */
	KEY_SWITCH_END=0,           /*!< Used as a parameter terminator to
	                                 keyNew() */
	
	KEY_SWITCH_INITIALIZED=0x10042008,
	KEY_SWITCH_INITMASK=0x18442218,
};



/*
 * Deprecated flag names, here for legacy compatibility.
 * Will be removed in the future.
 * Use KeySwitches instead.
 */
enum KeyFlags {                                       /* _DEPRECATED_ */
	KEY_FLAG_HASTYPE      = KEY_SWITCH_TYPE,          /* _DEPRECATED_ */
	KEY_FLAG_HASKEY       = KEY_SWITCH_NAME,          /* _DEPRECATED_ */
	KEY_FLAG_HASDATA      = KEY_SWITCH_VALUE,         /* _DEPRECATED_ */
	KEY_FLAG_HASDOMAIN    = KEY_SWITCH_DOMAIN,        /* _DEPRECATED_ */
	KEY_FLAG_HASCOMMENT   = KEY_SWITCH_COMMENT,       /* _DEPRECATED_ */
	KEY_FLAG_HASUID       = KEY_SWITCH_UID,           /* _DEPRECATED_ */
	KEY_FLAG_HASGID       = KEY_SWITCH_GID,           /* _DEPRECATED_ */
	KEY_FLAG_HASPRM       = KEY_SWITCH_MODE,           /* _DEPRECATED_ */
	KEY_FLAG_HASTIME      = KEY_SWITCH_TIME,          /* _DEPRECATED_ */
	KEY_FLAG_NEEDSYNC     = KEY_SWITCH_NEEDSYNC,      /* _DEPRECATED_ */
	KEY_FLAG_FLAG         = KEY_SWITCH_FLAG,          /* _DEPRECATED_ */
	KEY_FLAG_INITIALIZED  = KEY_SWITCH_INITIALIZED,   /* _DEPRECATED_ */
	KEY_FLAG_INITMASK     = KEY_SWITCH_INITMASK       /* _DEPRECATED_ */
};                                                    /* _DEPRECATED_ */



/**
 * Some return codes generated by the Elektra library.
 *
 * These are Elektra specific errors only, that the library sets in @c errno.
 * Other errors can be generated by system calls that the API uses.
 * Then @c errno is propagated.
 *
 * A very robust program should check @c errno after each API call.
 * @see kdbGetChildKeys() for an example on how to handle errors
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
	KDB_RET_TYPEMISMATCH=EILSEQ,  /*!< Failed to convert key data due to
	                                   data type */
	KDB_RET_INVALIDKEY=EAFNOSUPPORT, /*!< Key name is not @p 'system/something'
	                                      or @p 'user/something...' */
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
	KDB_O_RECURSIVE=1,       /*!< Act recursively. */
	KDB_O_DIR=1<<1,          /*!< Include dir keys in result. */
	KDB_O_NOVALUE=1<<2,      /*!< Retrieve only keys that don't have values
	                              (a.k.a dir keys). */
	KDB_O_NOEMPTY=1<<3,      /* unused ???? */
	KDB_O_STATONLY=1<<4,     /*!< Only stat key, instead of getting entirelly. */
	KDB_O_INACTIVE=1<<5,     /*!< Do not ignore inactive keys (that name begins
	                              with .). */
	KDB_O_SORT=1<<6,         /*!< Sort keys. */
	KDB_O_NFOLLOWLINK=1<<7,  /*!< Do not follow symlinks. */

/* XML exporting options for keyToStrem() */
	KDB_O_CONDENSED=1<<8,    /*!< Compressed XML, not usefull for human eyes. */
	KDB_O_NUMBERS=1<<9,      /*!< Use numbers intead of user and group names. */
	KDB_O_XMLHEADERS=1<<10,  /*!< Show also the XML header of the document. */
	KDB_O_FULLNAME=1<<11,    /*!< Export @p user keys using full name
	                              (e.g. user:username/some/key). */
	KDB_O_FULLUGID=1<<12,    /*!< Don't supress obvious key UID,
	                              GID, and user domain. Affects
	                              only @p user keys. */
	
/* Options used by ksLookupRE() methods */
	KDB_O_NOCASE=1<<15,      /*!< Ignore case in ksLookup*() methods */
	KDB_O_NOSPANPARENT=1<<16 /*!< Don't continue search if end of current
	                              folder reached, in ksLookupRE() */
};


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


/* The 'struct _Key' and 'struct _KeySet' will be removed from this file in
 * future versios of the library. The library is mature enough in terms of
 * methods to access each Key and KeySet attributes. So you should use them.
 * So you should never instantiate a Key or KeySet object like this:
 * 
 *    Key    myKey;      // don't do this
 *    KeySet myKeySet;   // don't do this
 *    keyInit(&myKey);   // don't do this
 *    ksInit(&myKeySet); // don't do this
 * 
 * You should always use pointers instead:
 * 
 *    Key    *myKey;
 *    KeySet *myKeySet;
 *    myKey=keyNew("user/some/key");
 *    myKeySet=ksNew();
 * 
 * This lets Elektra upgrades to not break previously compiled programs.
 * This is binary compatibility for your programs across different versions of
 * the library.
 * 
 * You can currently check if your sources are ready for this change. Simply
 * test a recompilation changing <kdb.h> to 
 * 
 *     #include "/usr/share/doc/elektra-devel/kdbfuture.h"
 * 
 * This include file is identical to kdb.h but without the hidden structs. So
 * kdbfuture.h will be used in place of kdb.h in future versions of the
 * library.
 * 
 */ 


struct _Key {                                                                    /* _DEPRECATED_ */
	/* All attributes are private. */                                            /* _DEPRECATED_ */
	/* Do not access them directly */                                            /* _DEPRECATED_ */
	 u_int8_t      type;        /* data type */                                  /* _DEPRECATED_ */
	    uid_t      uid;         /* owner user ID */                              /* _DEPRECATED_ */
	    uid_t      gid;         /* owner group ID */                             /* _DEPRECATED_ */
	   mode_t      access;      /* access control */                             /* _DEPRECATED_ */
	   time_t      atime;       /* time for last access */                       /* _DEPRECATED_ */
	   time_t      mtime;       /* time for last modification */                 /* _DEPRECATED_ */
	   time_t      ctime;       /* time for last change (meta info) */           /* _DEPRECATED_ */
	   size_t      commentSize; /* size of the description string */             /* _DEPRECATED_ */
	   size_t      dataSize;    /* size of the value */                          /* _DEPRECATED_ */
	   size_t      recordSize;  /* dataSize + commentSize + control */           /* _DEPRECATED_ */
	u_int32_t      flags;       /* Some control flags */                         /* _DEPRECATED_ */
	   char *      key;         /* The name of the key */                        /* _DEPRECATED_ */
	   char *      comment;     /* A comment about this key-value pair */        /* _DEPRECATED_ */
	   char *      userDomain;  /* User domain */                                /* _DEPRECATED_ */
	   void *      data;        /* The value */                                  /* _DEPRECATED_ */
	struct _Key * next;                                                          /* _DEPRECATED_ */
};                                                                               /* _DEPRECATED_ */







struct _KeySet {                        /* _DEPRECATED_ */
	/* All attributes are private. */   /* _DEPRECATED_ */
	/* Do not access them directly */   /* _DEPRECATED_ */
	struct _Key * start;                /* _DEPRECATED_ */
	struct _Key * end;                  /* _DEPRECATED_ */
	struct _Key * cursor;               /* _DEPRECATED_ */
	size_t        size;                 /* _DEPRECATED_ */
};                                      /* _DEPRECATED_ */




typedef struct _Key       Key;
typedef struct _KeySet    KeySet;




#ifdef __cplusplus
extern "C" {
#endif


/**************************************

KeyDB methods

***************************************/

int kdbOpen();
int kdbOpenBackend(char *backendName);
int kdbClose();

int kdbGetValue(const char *keyname, char *returned,size_t maxSize);
int kdbGetKeyByParent(const char *parentName, const char *baseName, Key *returned);
int kdbGetKeyByParentKey(const Key *parent, const char *baseName, Key *returned);
int kdbGetValueByParent(const char *parentName, const char *baseName, char *returned,
	size_t maxSize);

int kdbSetValue(const char *keyname, const char *value);
int kdbSetValueByParent(const char *parentName, const char *baseName, const char *value);

int kdbRename(Key *key, const char *newName);
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

int kdbGetKeyChildKeys(const Key *parentName, KeySet *returned, unsigned long options);
int kdbGetChildKeys(const char *parentName, KeySet *returned, unsigned long options);
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

Key *keyNew(const char *keyName, ...);
int keyDel(Key *key);
#define keyFree(x) keyDel(x)

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

size_t keyGetOwnerSize(const Key *key);
size_t keyGetOwner(const Key *key, char *returned, size_t maxSize);
size_t keySetOwner(Key *key, const char *userDomain);


size_t keyGetDataSize(const Key *key);

size_t keyGetString(const Key *key, char *returnedString, size_t maxSize);
size_t keySetString(Key *key, const char *newString);

size_t keyGetBinary(const Key *key, void *returnedBinary, size_t maxSize);
size_t keySetBinary(Key *key, const void *newBinary, size_t dataSize);

size_t keySetRaw(Key *key, const void *newBinary, size_t dataSize);

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

Key *keyNext(Key *key);

u_int32_t keyCompare(const Key *key1, const Key *key2);

size_t keyToStream(const Key *key, FILE* stream, unsigned long options);


/**************************************

KeySet methods

***************************************/

KeySet *ksNew();
int ksDel(KeySet *ks);
#define ksFree(x) ksDel(x)

int ksInit(KeySet *ks);
int ksClose(KeySet *ks);
size_t ksGetSize(KeySet *ks);

size_t ksInsert(KeySet *ks, Key *toInsert);
size_t ksAppend(KeySet *ks, Key *toAppend);
Key *ksPop(KeySet *ks);

size_t ksInsertKeys(KeySet *ks, KeySet *toInsert);
size_t ksAppendKeys(KeySet *ks, KeySet *toAppend);

size_t ksToStream(const KeySet *ks, FILE* stream, unsigned long options);
int ksCompare(KeySet *ks1, KeySet *ks2, KeySet *removed);
void ksSort(KeySet *ks);

Key *ksLookupByName(KeySet *ks, const char *name,unsigned long options);
Key *ksLookupByValue(KeySet *ks, const char *value,unsigned long options);
Key *ksLookupByBinaryValue(KeySet *ks, void *value, size_t size,
	unsigned long options);
u_int32_t ksLookupRE(KeySet *ks, u_int32_t where,
	const regex_t *regexp, unsigned long options);

int ksRewind(KeySet *ks);
Key *ksNext(KeySet *ks);
Key *ksCurrent(const KeySet *ks);

Key *ksHead(KeySet *ks);
Key *ksTail(KeySet *ks);


// Key *ksLookupByName(KeySet *ks,char *keyName);
// Key *ksLookupByRegex(KeySet *ks,regex_t *regex);




/***************************************

Helpers

***************************************/


size_t strblen(const char *s);

#ifdef __cplusplus
}
#endif


#endif /* KDB_H */

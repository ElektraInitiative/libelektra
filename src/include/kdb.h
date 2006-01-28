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
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


/* Subversion stuff

$Id$

*/

#ifndef KDB_H
#define KDB_H


/*
 * @defgroup general Elektra General definitions
 * @brief Some global definitions when using the Elektra API
 *
 */

/* Windows specific stuff */
#ifdef WIN32
#ifdef ELEKTRA_EXPORTS
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN /* Only defined if exporting, to allow applications to decide themselves if they want it */
  #endif
  #define ELEKTRA_API __declspec(dllexport)
#else
  #define ELEKTRA_API __declspec(dllimport)
#endif
#include <windows.h>
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
#define ssize_t int
#define strcasecmp stricmp
#define snprintf _snprintf
#else
#define ELEKTRA_API
#endif

#if !defined(WIN32)
/* The following file doesn't exist on windows so we need to avoid including it */
#include <inttypes.h>
#endif 

#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
/*#define NO_REGEX_H*/ /* Uncomment to disable regex */
/* Makes it possible to compile elektra without regex. 
 * For now this has to be defined manually. */
#ifndef NO_REGEX_H
#include <regex.h>
#endif 

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
 * @see kdbGetRootKeys(), keyGetNamespace(), keyNameGetNamespace()
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
.... .... 1... .... .... .... .... .... 0x00800000 ISSYSTEM
.... ...1 .... .... .... .... .... .... 0x01000000 ISUSER
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
	KEY_SWITCH_ISSYSTEM=1<<23,  /*!< Flag to denote a @c "system" key */
	KEY_SWITCH_ISUSER=1<<24,    /*!< Flag to denote a @c "user" key */
	KEY_SWITCH_FLAG=1<<31,      /*!< General purpose flag that has semantics
	                                 only to your app */
	KEY_SWITCH_END=0,           /*!< Used as a parameter terminator to
	                                 keyNew() */
	
	KEY_SWITCH_INITIALIZED=0x10042008,
	KEY_SWITCH_INITMASK=0x18442218
};



/*
 * Deprecated flag names, here for legacy compatibility.
 * Will be removed in the future.
 * Use KeySwitches above instead.
 */
enum KeyFlags {                                       /* _DEPRECATED_ */
	KEY_FLAG_HASTYPE      = KEY_SWITCH_TYPE,          /* _DEPRECATED_ */
	KEY_FLAG_HASKEY       = KEY_SWITCH_NAME,          /* _DEPRECATED_ */
	KEY_FLAG_HASDATA      = KEY_SWITCH_VALUE,         /* _DEPRECATED_ */
	KEY_FLAG_HASDOMAIN    = KEY_SWITCH_DOMAIN,        /* _DEPRECATED_ */
	KEY_FLAG_HASCOMMENT   = KEY_SWITCH_COMMENT,       /* _DEPRECATED_ */
	KEY_FLAG_HASUID       = KEY_SWITCH_UID,           /* _DEPRECATED_ */
	KEY_FLAG_HASGID       = KEY_SWITCH_GID,           /* _DEPRECATED_ */
	KEY_FLAG_HASPRM       = KEY_SWITCH_MODE,          /* _DEPRECATED_ */
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
 * The idea is to keep compatibility to POSIX @c errno system, so each library
 * error code maps to some POSIX E* error. Some mappings realy makes no sense,
 * so to detect Elektra errors use the following error names, and to detect
 * system's, use the naming convetions documented in @c errno man page.
 *
 * A very robust program should check @c errno after each API call.
 * @see kdbGetChildKeys() for an example on how to handle errors
 *
 * @ingroup kdb
 */
enum KDBErr {
	KDB_RET_OK=0,                 /*!< No error */


	/* Errors related to invalid/uninitialized objects */
	KDB_RET_NULLKEY=EINVAL,       /*!< Invalid Key object */
	KDB_RET_UNINITIALIZED=EINVAL, /*!< Object not initilized */

	/* Errors related to bad key names or keys not found */
	KDB_RET_NOTFOUND=ENOENT,      /*!< Key was not found */
	KDB_RET_INVALIDKEY=ESRCH, /*!< Key name is not @p 'system/something'
	                                      or @p 'user/something...' */

	/* Errors related to empty internal key properties */
	KDB_RET_NOKEY=ENXIO,        /*!< Key has no name */
	KDB_RET_NODATA=ENODEV,        /*!< Key has no data */
	KDB_RET_NODESC=ENOTDIR,         /*!< Key has no comment/description */
	KDB_RET_NODOMAIN=EDOM,        /*!< Key has no user domain set */
	KDB_RET_NOGROUP=ECHILD,       /*!< Key has no group */
	KDB_RET_NOTIME=ENOTTY,     /*!< Key has no access time set */


	/* Errors related to permissions, no memory or failed internal operations */
	KDB_RET_NOCRED=EACCES,        /*!< No credentials to access resource */
	KDB_RET_TRUNC=ERANGE,        /*!< Buffer was too small */
	KDB_RET_NOMEM=ENOMEM,         /*!< Out of memory */
	KDB_RET_TYPEMISMATCH=EBADF,  /*!< Failed to convert key data due to
	                                   data type */

	/* Errors related to backend access or opening */
	KDB_RET_NOSYS=ENOSYS,         /*!< Backend method not implemented */
	KDB_RET_EBACKEND=EIO          /*!< Error opening backend */
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
	KDB_O_DIRONLY=1<<2,      /*!< Retrieve only directory keys. */
	KDB_O_NOEMPTY=1<<3,      /* unused ???? */
	KDB_O_STATONLY=1<<4,     /*!< Only stat key, instead of getting entirelly.*/
	KDB_O_INACTIVE=1<<5,     /*!< Do not ignore inactive keys (that name begins
	                              with .). */
	KDB_O_SORT=1<<6,         /*!< Sort keys. */
	KDB_O_NFOLLOWLINK=1<<7,  /*!< Do not follow symlinks. */

/* XML exporting options for keyToStrem() and ksToStream() */
	KDB_O_CONDENSED=1<<8,    /*!< Compressed XML, not usefull for human eyes.*/
	KDB_O_NUMBERS=1<<9,      /*!< Use numbers intead of user and group names.*/
	KDB_O_XMLHEADERS=1<<10,  /*!< Show also the XML header of the document. */
	KDB_O_FULLNAME=1<<11,    /*!< Export @p user keys using full name
	                              (e.g. user:username/some/key). */
	KDB_O_FULLUGID=1<<12,    /*!< Don't supress obvious key UID,
	                              GID, and user domain. Affects
	                              only @p user keys. */
	KDB_O_HIER=1<<13,        /*!< Export to the new hierarchical XML
                                  representation using key basename.
                                  See ksToStream(). */

/* Options used by ksLookupRE() methods */
	KDB_O_NOCASE=1<<15,      /*!< Ignore case in ksLookup*() methods */
	KDB_O_NOSPANPARENT=1<<16,/*!< Don't continue search if end of current
                                  folder reached, in ksLookupRE() */

/* Obsolete/renamed options */
	KDB_O_NOVALUE=KDB_O_DIRONLY

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





typedef struct _Key       Key;
typedef struct _KeySet    KeySet;


/**
 * Object returned by kdbGetInfo() containing some informations about the
 * Elektra library and backend being used.
 *
 * You should not allocate or deallocate memory for each of these members.
 * This is a library responsability.
 *
 * @see kdbGetInfo(), kdbFreeInfo(), kdbInfoToString()
 * @see commandInfo() of the 'kdb info' command to see it in action
 * @ingroup kdb
 */
typedef struct _KDBInfo {
	/* all members are pointers because we'll point only to pre-allocated
	   or static strings. We won't allocate nothing for each member. */
	char *version;			/*!< Version of the library*/
	char *backendName;		/*!< Name of backend being or that will be used */
	uint8_t backendIsOpen;	/*!< 1 if backend was already open with kdbOpen(), 0 otherwise */
} KDBInfo;




#ifdef __cplusplus
extern "C" {
#endif


/**************************************

KeyDB methods

***************************************/

ELEKTRA_API int kdbOpen();
ELEKTRA_API int kdbOpenDefault();
ELEKTRA_API int kdbOpenBackend(char *backendName);
ELEKTRA_API int kdbClose();

ELEKTRA_API int kdbGetValue(const char *keyname, char *returned,size_t maxSize);
ELEKTRA_API int kdbGetKeyByParent(const char *parentName, const char *baseName, Key *returned);
ELEKTRA_API int kdbGetKeyByParentKey(const Key *parent, const char *baseName, Key *returned);
ELEKTRA_API int kdbGetValueByParent(const char *parentName, const char *baseName, char *returned,
	size_t maxSize);

ELEKTRA_API int kdbSetValue(const char *keyname, const char *value);
ELEKTRA_API int kdbSetValueByParent(const char *parentName, const char *baseName, const char *value);

ELEKTRA_API int kdbRename(Key *key, const char *newName);
ELEKTRA_API int kdbRemove(const char *keyName);
ELEKTRA_API int kdbRemoveKey(const Key *key);
ELEKTRA_API int kdbLink(const char *oldPath, const char *newKeyName);

ELEKTRA_API int kdbGetKeyByParent(const char *parentName, const char *baseName, Key *returned);
ELEKTRA_API int kdbGetKeyByParentKey(const Key *parent, const char *basename, Key *returned);
ELEKTRA_API int kdbGetValueByParent(const char *parentName, const char *baseName, char *returned,
	size_t maxSize);

ELEKTRA_API int kdbGetComment(const char *keyname, char *returned, size_t maxSize);
ELEKTRA_API size_t kdbSetComment(const char *keyname, const char *comment);

ELEKTRA_API int kdbStatKey(Key *key);
ELEKTRA_API int kdbGetKey(Key *key);
ELEKTRA_API int kdbSetKey(Key *key);

ELEKTRA_API ssize_t kdbGetKeyChildKeys(const Key *parentName, KeySet *returned, unsigned long options);
ELEKTRA_API ssize_t kdbGetChildKeys(const char *parentName, KeySet *returned, unsigned long options);
ELEKTRA_API ssize_t kdbGetRootKeys(KeySet *returned);

ELEKTRA_API int kdbSetKeys(KeySet *ks);

ELEKTRA_API uint32_t kdbMonitorKey(Key *interest, uint32_t diffMask,
	unsigned long iterations, unsigned usleep);
ELEKTRA_API uint32_t kdbMonitorKeys(KeySet *interests, uint32_t diffMask,
	unsigned long iterations, unsigned sleep);

ELEKTRA_API KDBInfo *kdbGetInfo(void);
ELEKTRA_API void kdbFreeInfo(KDBInfo *info);
ELEKTRA_API int kdbInfoToString(KDBInfo *info,char *string,size_t maxSize);



/**************************************

Key methods

***************************************/

ELEKTRA_API int keyInit(Key *key);
ELEKTRA_API int keyClose(Key *key);

ELEKTRA_API Key *keyNew(const char *keyName, ...);
ELEKTRA_API int keyDel(Key *key);
#define keyFree(x) keyDel(x)

ELEKTRA_API int keyIsInitialized(const Key *key);
ELEKTRA_API int keyNeedsSync(const Key *key);
ELEKTRA_API int keyDup(const Key *source,Key *dest);

/* int keySerialize(const Key *key,void *buffer, size_t maxSize);
int keyUnserialize(Key *key,const void *buffer); */
ELEKTRA_API size_t keyGetSerializedSize(const Key *key); 

ELEKTRA_API uint8_t keyGetType(const Key *key);
ELEKTRA_API uint8_t keySetType(Key *key,uint8_t type);

ELEKTRA_API int keySetFlag(Key *key);
ELEKTRA_API int keyClearFlag(Key *key);
ELEKTRA_API int keyGetFlag(const Key *key);

ELEKTRA_API ssize_t keyGetRecordSize(const Key *key);
ELEKTRA_API ssize_t keyGetNameSize(const Key *key);
ELEKTRA_API ssize_t keyGetFullNameSize(const Key *key);

ELEKTRA_API ssize_t keyGetName(const Key *key, char *returnedName, size_t maxSize);
ELEKTRA_API char *keyStealName(const Key *key);
ELEKTRA_API ssize_t keySetName(Key *key, const char *newName);

ELEKTRA_API ssize_t keyGetFullName(const Key *key, char *returnedName, size_t maxSize);
ELEKTRA_API ssize_t keyGetRootName(const Key *key, char *returned, size_t maxSize);
ELEKTRA_API ssize_t keyGetFullRootName(const Key *key, char *returned, size_t maxSize);

ELEKTRA_API ssize_t keyGetBaseName(const Key *key, char *returned, size_t maxSize);
ELEKTRA_API char *keyStealBaseName(const Key *key);
ELEKTRA_API ssize_t keyNameGetBaseNameSize(const char *keyName);
ELEKTRA_API ssize_t keyGetBaseNameSize(const Key *key);
ELEKTRA_API ssize_t keyAddBaseName(Key *key,const char *baseName);
ELEKTRA_API ssize_t keySetBaseName(Key *key,const char *baseName);

ELEKTRA_API ssize_t keyGetParentName(const Key *key, char *returned, size_t maxSize);
ELEKTRA_API ssize_t keyGetParentNameSize(const Key *key);

ELEKTRA_API ssize_t keyNameGetRootNameSize(const char *keyName);
ELEKTRA_API ssize_t keyGetRootNameSize(const Key *key);
ELEKTRA_API ssize_t keyGetFullRootNameSize(const Key *key);


ELEKTRA_API ssize_t keyGetCommentSize(const Key *key);
ELEKTRA_API ssize_t keyGetComment(const Key *key, char *returnedDesc, size_t maxSize);
ELEKTRA_API char *keyStealComment(const Key *key);
ELEKTRA_API ssize_t keySetComment(Key *key, const char *newDesc);

ELEKTRA_API uid_t keyGetUID(const Key *key);
ELEKTRA_API int keySetUID(Key *key, uid_t uid);

ELEKTRA_API gid_t keyGetGID(const Key *key);
ELEKTRA_API int keySetGID(Key *key, gid_t gid);

ELEKTRA_API mode_t keyGetAccess(const Key *key);
ELEKTRA_API int keySetAccess(Key *key, mode_t mode);

ELEKTRA_API ssize_t keyGetOwnerSize(const Key *key);
ELEKTRA_API ssize_t keyGetOwner(const Key *key, char *returned, size_t maxSize);
ELEKTRA_API char *keyStealOwner(const Key *key);
ELEKTRA_API ssize_t keySetOwner(Key *key, const char *userDomain);


ELEKTRA_API ssize_t keyGetValueSize(const Key *key);
ELEKTRA_API ssize_t keyGetDataSize(const Key *key);

ELEKTRA_API ssize_t keyGetString(const Key *key, char *returnedString, size_t maxSize);
ELEKTRA_API ssize_t keySetString(Key *key, const char *newString);
ELEKTRA_API void *keyStealValue(const Key *key);

ELEKTRA_API ssize_t keyGetBinary(const Key *key, void *returnedBinary, size_t maxSize);
ELEKTRA_API ssize_t keySetBinary(Key *key, const void *newBinary, size_t dataSize);

ELEKTRA_API ssize_t keySetRaw(Key *key, const void *newBinary, size_t dataSize);

ELEKTRA_API ssize_t keyGetLink(const Key *key, char *returnedTarget, size_t maxSize);
ELEKTRA_API ssize_t keySetLink(Key *key, const char *target);

ELEKTRA_API time_t keyGetMTime(const Key *key);
ELEKTRA_API time_t keyGetATime(const Key *key);
ELEKTRA_API time_t keyGetCTime(const Key *key);

ELEKTRA_API int keyIsSystem(const Key *key);
ELEKTRA_API int keyNameIsSystem(const char *keyName);

ELEKTRA_API int keyIsUser(const Key *key);
ELEKTRA_API int keyNameIsUser(const char *keyName);

ELEKTRA_API int keyGetNamespace(const Key *key);
ELEKTRA_API int keyNameGetNamespace(const char *keyName);

ELEKTRA_API int keyIsDir(const Key *key);
ELEKTRA_API int keyIsLink(const Key *key);
ELEKTRA_API int keyIsBin(const Key *key);

ELEKTRA_API Key *keyNext(Key *key);

ELEKTRA_API uint32_t keyCompare(const Key *key1, const Key *key2);

ELEKTRA_API ssize_t keyToStream(const Key *key, FILE* stream, unsigned long options);
ELEKTRA_API ssize_t keyToStreamBasename(const Key *key, FILE* stream,
	const char *parent, const size_t parentSize, unsigned long options);


/**************************************

KeySet methods

***************************************/

ELEKTRA_API KeySet *ksNew();
ELEKTRA_API int ksDel(KeySet *ks);
#define ksFree(x) ksDel(x)

ELEKTRA_API int ksInit(KeySet *ks);
ELEKTRA_API int ksClose(KeySet *ks);
ELEKTRA_API ssize_t ksGetSize(KeySet *ks);

ELEKTRA_API ssize_t ksInsert(KeySet *ks, Key *toInsert);
ELEKTRA_API ssize_t ksAppend(KeySet *ks, Key *toAppend);
ELEKTRA_API Key *ksPop(KeySet *ks);
ELEKTRA_API Key *ksPopLast(KeySet *ks);

ELEKTRA_API ssize_t ksInsertKeys(KeySet *ks, KeySet *toInsert);
ELEKTRA_API ssize_t ksAppendKeys(KeySet *ks, KeySet *toAppend);

ELEKTRA_API ssize_t ksToStream(const KeySet *ks, FILE* stream, unsigned long options);
ELEKTRA_API int ksCompare(KeySet *ks1, KeySet *ks2, KeySet *removed);
ELEKTRA_API void ksSort(KeySet *ks);

ELEKTRA_API Key *ksLookupByName(KeySet *ks, const char *name,unsigned long options);
ELEKTRA_API Key *ksLookupByValue(KeySet *ks, const char *value,unsigned long options);
ELEKTRA_API Key *ksLookupByBinaryValue(KeySet *ks, void *value, size_t size,
	unsigned long options);
ELEKTRA_API uint32_t ksLookupRE(KeySet *ks, uint32_t where,
	const regex_t *regexp, unsigned long options);
	
ELEKTRA_API int ksRewind(KeySet *ks);
ELEKTRA_API Key *ksNext(KeySet *ks);
ELEKTRA_API Key *ksCurrent(const KeySet *ks);

ELEKTRA_API Key *ksHead(KeySet *ks);
ELEKTRA_API Key *ksTail(KeySet *ks);


/* Key *ksLookupByName(KeySet *ks,char *keyName); */
/* Key *ksLookupByRegex(KeySet *ks,regex_t *regex); */




/***************************************

Helpers

***************************************/


ELEKTRA_API size_t strblen(const char *s);

#ifdef __cplusplus
}
#endif


#endif /* KDB_H */

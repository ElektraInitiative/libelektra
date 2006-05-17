/***************************************************************************
      kdbprivate.h  -  Private stuff for the kdb implementation
                             -------------------
    begin                : Mon Apr 12 2004
    copyright            : (C) 2004 by Avi Alkalay
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

#ifndef KDBPRIVATE_H
#define KDBPRIVATE_H

/* Elektra data directories */
#define KDB_DB_SYSTEM            "/etc/kdb"
#define KDB_DB_USER              ".kdb"   /* $HOME/.kdb */



#define BUFFER_SIZE 100

#ifdef UT_NAMESIZE
#define USER_NAME_SIZE UT_NAMESIZE
#else
#define USER_NAME_SIZE 100
#endif


#ifndef DEFFILEMODE
#define DEFFILEMODE 0666
#endif



#define UTF8_TO   1
#define UTF8_FROM 0



/* These define the type for pointers to all the kdb functions */
typedef int      (*kdbOpenPtr)(KDBHandle *);
typedef int      (*kdbClosePtr)(KDBHandle *);
typedef int      (*kdbStatKeyPtr)(KDBHandle, Key *);
typedef int      (*kdbGetKeyPtr)(KDBHandle, Key *);
typedef int      (*kdbSetKeyPtr)(KDBHandle, Key *);
typedef int      (*kdbRenamePtr)(KDBHandle, Key *, const char *);
typedef int      (*kdbRemoveKeyPtr)(KDBHandle, const Key *);
typedef int      (*kdbGetChildKeysPtr)(KDBHandle, const Key *, KeySet *,
                 unsigned long);

typedef int      (*kdbSetKeysPtr)(KDBHandle, KeySet *);

typedef uint32_t (*kdbMonitorKeyPtr)(KDBHandle, Key *, uint32_t,
                 unsigned long, unsigned);

typedef uint32_t (*kdbMonitorKeysPtr)(KDBHandle, KeySet *, uint32_t,
                 unsigned long, unsigned);



 
 
/**
 * The private Key struct.
 * 
 * Its internal private attributes should not be accessed directly by regular
 * programs. Use the @ref key "Key access methods" instead.
 * Only a backend writer needs to have access to the private attributes of the
 * Key object which is defined as:
 * @code
typedef struct _Key Key;
 * @endcode
 * 
 * @ingroup backend
 */
struct _Key {
	/**
	 * Type of the value, from #KeyType.
	 * @see keyGetType(), keySetType(), keyIsBin()
	 */ 
	uint8_t       type;
	 
	/**
	 * System UID of this key.
	 * @see keyGetUID(), keySetUID()
	 */ 
	uid_t          uid;
	 
	/**
	 * System GID of this key.
	 * @see keyGetGID(), keySetGID()
	 */ 
	uid_t          gid;
	 
	/**
	 * File-like access control
	 * @see keyGetAccess(), keySetAccess()
	 */ 
	mode_t         access;
	 
	/**
	 * Time for last access (stat).
	 * @see keyGetATime()
	 */ 
	time_t         atime;

	/**
	 * Time for last modification.
	 * @see keyGetMTime()
	 */
	time_t         mtime;
	 
	/**
	 * Time for last change (meta info)
	 * @see keyGetCTime()
	 */ 
	time_t         ctime;
	 
	/**
	 * Size of the comment of description string, including ending NULL.
	 * @see keyGetCommentSize(), keySetComment(), keyGetComment()
	 */ 
	size_t         commentSize;
	 
	/**
	 * Size of the value, in bytes, including ending NULL.
	 * @see keyGetCommentSize(), keySetComment(), keyGetComment()
	 */ 
	size_t         dataSize;
	size_t         recordSize;  /**< dataSize + commentSize + some control */
	
	/**
	 * Some control and internal flags.
	 * @see keySetFlag(), keyGetFlag()
	 */ 
	uint32_t      flags;
	 
	/**
	 * The name of the key.
	 * @see keySetName(), keyGetName()
	 */ 
	char *         key;
	 
	/**
	 * A comment about the key.
	 * @see keySetComment(), keyGetComment()
	 */ 
	char *         comment;
	 
	/**
	 * The user that owns the key.
	 * @see keySetComment(), keyGetComment()
	 */ 
	char *         userDomain;
	
	/**
	 * The user that owns the key.
	 * @see keySetString(), keyGetString()
	 */ 
	void *         data;        /**< The value, which is a NULL terminated string or binary */
	struct _Key *  next;        /**< Link to the next object in a KeySet context */
};


/**
 * Macro to calculates the real size in bytes of the metainfo of the Key struct.
 * The metainfo part of the struct is composed by _Key::type, _Key::uid,
 * _Key::gid, _Key::access, _Key::atime, _Key::mtime, _Key::ctime,
 * _Key::commentSize, _Key::dataSize.
 * 
 * This macro is usefull in Key serialization and de-serialization methods
 * of your backend.
 * @ingroup backend
 */
#define KEY_METAINFO_SIZE(k) ((unsigned int)&(k->recordSize) - (unsigned int)k)



/**
 * The private KeySet structure.
 *
 * Its internal private attributes should not be accessed directly by regular
 * programs. Use the @ref keyset "KeySet access methods" instead.
 * Only a backend writer needs to have access to the private attributes of the
 * KeySet object which is defined as:
 * @code
typedef struct _KeySet KeySet;
 * @endcode
 * 
 * @ingroup backend
 */
struct _KeySet {
	struct _Key * start;   /**< First key on the list */
	struct _Key * end;     /**< Last key on the list */
	struct _Key * cursor;  /**< Internal cursor */
	size_t        size;    /**< Number of keys contained in the KeySet */
};






#ifdef __cplusplus
extern "C" {
#endif



ssize_t encode(void *unencoded, size_t size, char *returned);
ssize_t unencode(char *encoded, void *returned);

int kdbNeedsUTF8Conversion();
int UTF8Engine(int direction, char **string, size_t *inputByteSize);




#ifdef __cplusplus
}
#endif




#endif /* KDBPRIVATE_H */

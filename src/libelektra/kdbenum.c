/*This is a dummy file do document the enums which have
 * no name in the header file. They are duplicated here
 * to document them. */

/**
 * Switches to denote the various Key attributes in methods throughout
 * this library.
 *
 * This enum switch provide a flag for every metadata in a key.
 *
 * In case of keyNew() they give Information what Parameter comes
 * next.
 *
 * @ingroup key
 * @see keyNew()
 * @see ksToStream(), keyToStream()
 */
enum keyswitch_t
{
	KEY_NAME=1,		/*!< Flag for the key name */
	KEY_VALUE=1<<1,		/*!< Flag for the key data */
	KEY_OWNER=1<<2,		/*!< Flag for the key user domain */
	KEY_COMMENT=1<<3,	/*!< Flag for the key comment */
	KEY_BINARY=1<<4,	/*!< Flag if the key is binary */
	KEY_UID=1<<5,		/*!< Flag for the key UID */
	KEY_GID=1<<6,		/*!< Flag for the key GID */
	KEY_MODE=1<<7,		/*!< Flag for the key permissions */
	KEY_ATIME=1<<8,		/*!< Flag for the key access time */
	KEY_MTIME=1<<9,		/*!< Flag for the key change time */
	KEY_CTIME=1<<10,	/*!< Flag for the key status change time */
	KEY_SIZE=1<<11,		/*!< Flag for maximum size to limit value */
	KEY_DIR=1<<14,		/*!< Flag for the key directories*/
	KEY_END=0		/*!< Used as a parameter terminator to keyNew() */
};

/**
 * End of a list of keys.
 *
 * Use this macro to define the end of a variable-length list
 * of keys.
 *
 * @def KS_END
 * @see ksNew() and ksVNew()
 */
#define KS_END ((Key*)0)

/**
 * Options to change the default behavior of kdbGet(), kdbSet()
 * and ksLookup() functions.
 *
 * These options can be ORed. That is the |-Operator in C.
 *
 * @ingroup kdb
 * @see kdbGet(), kdbSet()
 */
enum option_t
{

/**
 * No Option set.
 * Will be recursive with no inactive keys.
 *
 * @see kdbGet(), kdbSet(), ksLookup()
 */
	KDB_O_NONE=0,
/**
 * Delete parentKey key in kdbGet(), kdbSet() or ksLookup().
 *
 * @see kdbGet(), kdbSet()
 */
	KDB_O_DEL=1,
/** Pop Parent out of keyset key in kdbGet().
 *
 * @see ksPop().
 */
	KDB_O_POP=1<<1,
/** Exclude keys containing other keys in result.
 *
 * Only return leaves.
 *
 * @see keyIsDir() 
 */
	KDB_O_NODIR=1<<2,
/** Retrieve only directory keys
      (keys containing other keys).
      This will give you an skeleton without leaves.
      This must not be used together with KDB_O_NODIR.
      @see keyIsDir() */
	KDB_O_DIRONLY=1<<3,
/** Don't remove any keys.
      This must not be used together with KDB_O_REMOVEONLY.
      */
	KDB_O_NOREMOVE=1<<6,
/** Only remove keys.
      This must not be used together with KDB_O_NOREMOVE.
      */
	KDB_O_REMOVEONLY=1<<7,
/** Do not ignore inactive keys (that name begins
      with .).
      @see keyIsInactive() */
	KDB_O_INACTIVE=1<<8,
/** Set keys independent of sync status.
      @see keyNeedSync() */
	KDB_O_SYNC=1<<9,
/** This option has no effect.
      KeySets are always sorted.
      @deprecated don't use */
	KDB_O_SORT=1<<10,
/** Do not call kdbGet() for every key
      containing other keys (keyIsDir()). */
	KDB_O_NORECURSIVE=1<<11,
/** Ignore case. */
	KDB_O_NOCASE=1<<12,
/** Search with owner. */
	KDB_O_WITHOWNER=1<<13,
/** Only search from start -> cursor to cursor -> end. */
	KDB_O_NOALL=1<<14
};


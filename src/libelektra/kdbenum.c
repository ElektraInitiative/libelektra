/*This is a dummy file do document the enums which have
 * no name in the header file. They are duplicated here
 * to document them. */

/**
 * Key type.
 *
 * The type number is a value between 0 and 255 defined in an unnamed enum.
 * Key type values grow from the semantically poor to the semantically rich.
 * The gaps between them is for user-defined types.
 *
 * If your application needs value types with more semantics, like @p Color,
 * @p Font, etc, you can still use it. You'll have to define a new type
 * number in the scope of your application, and set the type with
 * keySetType() or keyNew().
 *
 * @p KEY_TYPE_STRING <= type < @p KEY_TYPE_MAX will be treated as a string
 * (in the terms of Unicode handling and not allowing '\\0').
 * @see keyIsString()
 *
 * @p KEY_TYPE_BINARY <= type < @p KEY_TYPE_STRING will be handled
 * as a binary value and will not make any conversations.
 * @see keyIsBinary()
 *
 * The elektra projects will define more types in the future, so you might
 * participate at the process otherwise your types may be overlapping. For
 * these types the range from 100 to 200 is reserved.
 *
 * @ingroup key
 * @see keyGetType()
 * @see keySetType() for an example of how to define custom types
 */
enum type_t
{
	KEY_TYPE_UNDEFINED=0,	/*!< Undefined key.
		Data will not be considered. That means the value will
		not be written out to disc and will be lost!*/
	KEY_TYPE_BINARY=20,	/*!< Binary key.
		This gap is for binary data types
		that have some semantics that somebody
		can invent in the future */
	KEY_TYPE_STRING=40,	/*!< String key.
		All types below this type are some type of strings.
		They can be used for internal usage or a type system.*/
	KEY_TYPE_MAX=256	/*!< This type does not exists,
		use values below. */
};

/**
 * Switches to denote the various Key attributes in methods throughout
 * this library.
 *
 * This enum switch provide a flag for every metadata in a key.
 *
 * In case of keyNew() they give Information what Parameter comes
 * next.
 *
 * In case of ksToStream() the additional information will be typed
 * out. Use them together with #KDBStream in that situation.
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
	KEY_TYPE=1<<4,		/*!< Flag for the key type */
	KEY_UID=1<<5,		/*!< Flag for the key UID */
	KEY_GID=1<<6,		/*!< Flag for the key GID */
	KEY_MODE=1<<7,		/*!< Flag for the key permissions */
	KEY_ATIME=1<<8,		/*!< Flag for the key access time */
	KEY_MTIME=1<<9,		/*!< Flag for the key change time */
	KEY_CTIME=1<<10,	/*!< Flag for the key status change time */
	KEY_SIZE=1<<11,		/*!< Flag for maximum size to limit value */
	KEY_REMOVE=1<<12,	/*!< Flag if key is marked for remove */
	KEY_STAT=1<<13,		/*!< Flag if key is marked for stat */
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
/** Don't stat keys, get them. No key will be true on
      keyNeedStat() afterwards. You will receive
      value, comment and key type regardless of keyNeedStat().
      @see keyNeedStat() */
	KDB_O_NOSTAT=1<<4,
/** Only stat the keys, don't receive value, comment or key type.
      This must not be used together with KDB_O_NOSTAT.
      @see keyNeedStat() */
	KDB_O_STATONLY=1<<5,
/** Don't remove keys, set them. No key will be true on
      keyNeedRemove() afterwards.
      @see keyNeedRemove() */
	KDB_O_NOREMOVE=1<<6,
/** Only remove keys, don't set them.
      This must not be used together with KDB_O_NOREMOVE.
      @see keyNeedRemove() */
	KDB_O_REMOVEONLY=1<<7,
/** Do not ignore inactive keys (that name begins
      with .).
      @see keyIsInactive() */
	KDB_O_INACTIVE=1<<8,
/** Set keys independent of sync status
      @see keyNeedSync() */
	KDB_O_SYNC=1<<9,
/** Force sorting instead of sorting when needed.
      You will get back a sorted keyset for iteration.
      @see ksNeedSort() */
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


/**
 * @file
 *
 * @brief dummy file do document the enums which have no name in the header file.
 *
 * They are duplicated here to document them.
 *
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

/**
  * The version information in x.y.z format as string.
  *
  * To get the version at run-time, you can get the key
  * system:/elektra/version/constants/KDB_VERSION
  *
  * @see [VERSION.md](/doc/VERSION.md).
  * @see #KDB_VERSION_MAJOR
  * @see #KDB_VERSION_MINOR
  * @see #KDB_VERSION_PATCH
  * @ingroup kdb
  */
#define KDB_VERSION "x.y.z"

/**
  * The version information of the major version as number.
  *
  * To get the version at run-time, you can get the key
  * system:/elektra/version/constants/KDB_VERSION_MAJOR
  *
  * @see [VERSION.md](/doc/VERSION.md).
  * @see #KDB_VERSION
  * @ingroup kdb
  */
#define KDB_VERSION_MAJOR x

/**
  * The version information of the minor version as number.
  *
  * To get the version at run-time, you can get the key
  * system:/elektra/version/constants/KDB_VERSION_MINOR
  *
  * @see [VERSION.md](/doc/VERSION.md).
  * @see #KDB_VERSION
  * @ingroup kdb
  */
#define KDB_VERSION_MINOR y

/**
  * The version information of the patch version as number.
  *
  * To get the version at run-time, you can get the key
  * system:/elektra/version/constants/KDB_VERSION_PATCH
  *
  * @see [VERSION.md](/doc/VERSION.md).
  * @see #KDB_VERSION
  * @ingroup kdb
  */
#define KDB_VERSION_PATCH z

/** `/` is used to separate key names.
 *
 * @see @link keyname description about key names @endlink.
  * @ingroup key
 * */
#define KDB_PATH_SEPARATOR '/'

/** `\` is used as escape character in the key name.
 *
 * @see @link keyname description about key names @endlink.
 * @ingroup key
 * */
#define KDB_PATH_ESCAPE '\\'

/** If optimizations are enabled in Elektra */
#define ELEKTRA_ENABLE_OPTIMIZATIONS

/**
 * Allows keyNew() to determine which information comes next.
 *
 * @ingroup key
 * @see keyNew()
 */
enum elektraKeyFlags
{
	KEY_NAME=1,		/*!< Flag for the key name */
	KEY_VALUE=1<<1,		/*!< Flag for the key data */
	KEY_FLAGS=3,		/*!< Allows to define multiple flags at once. */
	KEY_OWNER=1<<2,		/*!< Flag for the key user domain */
	KEY_COMMENT=1<<3,	/*!< Flag for the key comment */
	KEY_BINARY=1<<4,	/*!< Flag if the key is binary */
	KEY_UID=1<<5,		/*!< Flag for the key UID @deprecated do not use */
	KEY_GID=1<<6,		/*!< Flag for the key GID @deprecated do not use  */
	KEY_MODE=1<<7,		/*!< Flag for the key permissions @deprecated do not use  */
	KEY_ATIME=1<<8,		/*!< Flag for the key access time @deprecated do not use  */
	KEY_MTIME=1<<9,		/*!< Flag for the key change time @deprecated do not use  */
	KEY_CTIME=1<<10,	/*!< Flag for the key status change time @deprecated do not use  */
	KEY_SIZE=1<<11,		/*!< Flag for maximum size to limit value */
	KEY_DIR=1<<14,		/*!< Flag for the key directories @deprecated do not use */
	KEY_META=1<<15,		/*!< Flag for metadata */
	KEY_NULL=1<<16,		/*!< Is *not* a flag, only as return value @deprecated do not use */
	// hole for elektraLockFlags
	KEY_CASCADING_NAME=1<<20,	/*!< Is default, no need to use it @deprecated do not use */
	KEY_META_NAME=1<<21,	/*!< Allow any key names (not only with known namespaces+cascading */
	KEY_END=0		/*!< Used as a parameter terminator to keyNew() */
};

/**
 * Lock options
 *
 * @ingroup key
 * @see keyLock(), keyIsLocked()
 */
enum elektraLockFlags
{
	KEY_LOCK_NAME=1<<17,	/*!< lock the name of a key */
	KEY_LOCK_VALUE=1<<18,	/*!< lock the value of a key */
	KEY_LOCK_META=1<<19,	/*!< lock the meta data of a key */
};


/**
 * Elektra currently supported Key namespaces.
 *
 * @ingroup key
 * @see kdbGet(), keyGetNamespace()
 */
enum elektraNamespace
{
	KEY_NS_NONE=0,            ///< no key given as parameter to keyGetNamespace()
	KEY_NS_CASCADING=1,       ///< cascading key, starts with /, abstract name for any of the namespaces below
	KEY_NS_META=2,            ///< metakey, i.e. any key name not under other categories
	KEY_NS_SPEC=3,            ///< spec contains the specification of the other namespaces
	KEY_NS_PROC=40,           ///< proc contains process-specific configuration
	KEY_NS_DIR=80,            ///< dir contains configuration from a specific directory
	KEY_NS_USER=120,          ///< user key in the home directory of the current user
	KEY_NS_SYSTEM=160,        ///< system key is shared for a computer system
  KEY_NS_DEFAULT=200,       ///< default key used as a fallback if no other key is found
};

/**
 * End of a list of keys.
 *
 * Use this macro to define the end of a variable-length list
 * of keys.
 *
 * @def KS_END
 * @see ksNew() and ksVNew()
  * @ingroup keyset
 */
#define KS_END ((Key*)0)

/**
 * Options to change the default behavior of
 * ksLookup() functions.
 *
 * These options can be ORed. That is the |-Operator in C.
 *
 * @ingroup keyset
 * @see kdbGet(), kdbSet()
 */
enum elektraLookupFlags
{

/**
 * No Option set.
 *
 * @see ksLookup()
 */
	KDB_O_NONE=0,
/**
 * Delete parentKey key in ksLookup().
 *
 * @see ksLookup()
 */
	KDB_O_DEL=1,
/** Pop Parent out of keyset key in ksLookup().
 *
 * @see ksPop().
 */
	KDB_O_POP=1<<1
};


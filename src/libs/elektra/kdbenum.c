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
	ELEKTRA_KEY_NAME = 1,	      /*!< Flag for the key name */
	ELEKTRA_KEY_VALUE = 1 << 1,   /*!< Flag for the key data */
	ELEKTRA_KEY_FLAGS = 3,	      /*!< Allows to define multiple flags at once. */
	ELEKTRA_KEY_COMMENT = 1 << 3, /*!< Flag for the key comment */
	ELEKTRA_KEY_BINARY = 1 << 4,  /*!< Flag if the key is binary */
	ELEKTRA_KEY_SIZE = 1 << 11,   /*!< Flag for maximum size to limit value */
	ELEKTRA_KEY_META = 1 << 15,   /*!< Flag for metadata */
	ELEKTRA_KEY_NULL = 1 << 16,   /*!< Is *not* a flag, only as return value @deprecated do not use */
	// hole for elektraLockFlags
	ELEKTRA_KEY_END = 0 /*!< Used as a parameter terminator to keyNew() */
};

/**
 * Copy options
 *
 * @ingroup key
 * @see keyCopy()
 */
enum elektraCopyFlags
{
	ELEKTRA_KEY_CP_NAME = 1 << 0,				       /*!< Flag for copying the key name */
	ELEKTRA_KEY_CP_STRING = 1 << 1,				       /*!< Flag for copying the key value, if it is a string */
	ELEKTRA_KEY_CP_VALUE = 1 << 2,				       /*!< Flag for copying the key value */
	ELEKTRA_KEY_CP_META = 1 << 3,				       /*!< Flag for copying the key metadata */
	ELEKTRA_KEY_CP_ALL = ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE | ELEKTRA_KEY_CP_META, /*!< Shorthand for copying name, value and metadata */
};

/**
 * Lock options
 *
 * @ingroup key
 * @see keyLock(), keyIsLocked()
 */
enum elektraLockFlags
{
	ELEKTRA_KEY_LOCK_NAME = 1 << 17,  /*!< lock the name of a key */
	ELEKTRA_KEY_LOCK_VALUE = 1 << 18, /*!< lock the value of a key */
	ELEKTRA_KEY_LOCK_META = 1 << 19,  /*!< lock the meta data of a key */
};


/**
 * Elektra currently supported Key namespaces.
 *
 * @ingroup key
 * @see kdbGet(), keyGetNamespace()
 */
enum elektraNamespace
{
	ELEKTRA_NS_NONE = 0,      ///< no key given as parameter to keyGetNamespace()
	ELEKTRA_NS_CASCADING = 1, ///< cascading key, starts with /, abstract name for any of the namespaces below
	ELEKTRA_NS_META = 2,      ///< metakey, i.e. any key name not under other categories
	ELEKTRA_NS_SPEC = 3,      ///< spec contains the specification of the other namespaces
	ELEKTRA_NS_PROC = 4,      ///< proc contains process-specific configuration
	ELEKTRA_NS_DIR = 5,	      ///< dir contains configuration from a specific directory
	ELEKTRA_NS_USER = 6,      ///< user key in the home directory of the current user
	ELEKTRA_NS_SYSTEM = 7,    ///< system key is shared for a computer system
	ELEKTRA_NS_DEFAULT = 8,   ///< default key used as a fallback if no other key is found
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
#define ELEKTRA_KS_END ((Key *) 0)

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
	ELEKTRA_KDB_O_NONE = 0,
	/**
	 * Delete parentKey key in ksLookup().
	 *
	 * @see ksLookup()
	 */
	ELEKTRA_KDB_O_DEL = 1,
	/** Pop Parent out of keyset key in ksLookup().
	 *
	 * @see ksPop().
	 */
	ELEKTRA_KDB_O_POP = 1 << 1
};

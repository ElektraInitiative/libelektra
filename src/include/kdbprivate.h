/**
 * @file
 *
 * @brief Private declarations.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDBPRIVATE_H
#define KDBPRIVATE_H

#include <kdb.h>
#include <kdbconfig.h>
#include <kdbextension.h>
#include <kdbhelper.h>
#include <kdbplugin.h>
#include <kdbproposal.h>
#include <kdbtypes.h>
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
#include <kdbopmphm.h>
#endif

#include <limits.h>

/** The minimal allocation size of a keyset inclusive
	NULL byte. ksGetAlloc() will return one less because
	it says how much can actually be stored.*/
#define KEYSET_SIZE 16

/** How many plugins can exist in an backend. */
#define NR_OF_PLUGINS 10

/** The index of the commit plugin */
#define COMMIT_PLUGIN 7

/** The index of the storage plugin */
#define STORAGE_PLUGIN 5

/** The index of the resolver plugin */
#define RESOLVER_PLUGIN 0

/** Trie optimization */
#define APPROXIMATE_NR_OF_BACKENDS 16

/**The maximum of how many characters an integer
  needs as decimal number.*/
#define MAX_LEN_INT 31

/**Backend mounting information.
 *
 * This key directory tells you where each backend is mounted
 * to which mountpoint. */
#define KDB_SYSTEM_ELEKTRA "system/elektra"

#if DEBUG
#include <stdio.h>
#define ELEKTRA_PRINT_DEBUG(text) printf ("%s:%d: %s\n", __FILE__, __LINE__, text);
#else
#define ELEKTRA_PRINT_DEBUG(text)
#endif

#if DEBUG && VERBOSE
#define ELEKTRA_PRINT_VERBOSE(text) printf ("%s:%d: %s\n", __FILE__, __LINE__, text);
#else
#define ELEKTRA_PRINT_VERBOSE(text)
#endif

#if DEBUG || defined(ELEKTRA_BMC)
#undef NDEBUG
#include <assert.h>
#define ELEKTRA_ASSERT(NX) assert (NX)
#else
#define ELEKTRA_ASSERT(NX)
#endif


#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

typedef struct _Trie Trie;
typedef struct _Split Split;
typedef struct _Backend Backend;

/* These define the type for pointers to all the kdb functions */
typedef int (*kdbOpenPtr) (Plugin *, Key * errorKey);
typedef int (*kdbClosePtr) (Plugin *, Key * errorKey);

typedef int (*kdbGetPtr) (Plugin * handle, KeySet * returned, Key * parentKey);
typedef int (*kdbSetPtr) (Plugin * handle, KeySet * returned, Key * parentKey);
typedef int (*kdbErrorPtr) (Plugin * handle, KeySet * returned, Key * parentKey);


typedef Backend * (*OpenMapper) (const char *, const char *, KeySet *);
typedef int (*CloseMapper) (Backend *);


/*****************
 * Key Flags
 *****************/

enum
{
	KEY_EMPTY_NAME = 1 << 22
};

// clang-format off

/**
 * Key Flags.
 *
 * Store a synchronizer state so that the Elektra knows if something
 * has changed or not.
 *
 * @ingroup backend
 */
typedef enum {
	KEY_FLAG_SYNC = 1,	  /*!<
			Key need sync.
			If name, value or metadata
			are changed this flag will be set, so that the backend will sync
			the key to database.*/
	KEY_FLAG_RO_NAME = 1 << 1,	/*!<
			 Read only flag for name.
			 Key name is read only and not allowed
			 to be changed. All attempts to change the name
			 will lead to an error.
			 Needed for meta keys and keys that are in a data
			 structure that depends on name ordering.*/
	KEY_FLAG_RO_VALUE = 1 << 2, /*!<
			 Read only flag for value.
			 Key value is read only and not allowed
			 to be changed. All attempts to change the value
			 will lead to an error.
			 Needed for meta keys*/
	KEY_FLAG_RO_META = 1 << 3	/*!<
			 Read only flag for meta.
			 Key meta is read only and not allowed
			 to be changed. All attempts to change the value
			 will lead to an error.
			 Needed for meta keys.*/
} keyflag_t;


/**
 * Ks Flags.
 *
 * Store a synchronizer state so that the Elektra knows if something
 * has changed or not.
 *
 * @ingroup backend
 */
typedef enum {
	KS_FLAG_SYNC = 1 /*!<
		 KeySet need sync.
		 If keys were popped from the Keyset
		 this flag will be set, so that the backend will sync
		 the keys to database.*/
} ksflag_t;


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
struct _Key
{
	/**
	 * The value, which is a NULL terminated string or binary.
	 * @see keyString(), keyBinary(),
	 * @see keyGetString(), keyGetBinary(),
	 * @see keySetString(), keySetBinary()
	 */
	union {
		char * c;
		void * v;
	} data;

	/**
	 * Size of the value, in bytes, including ending NULL.
	 * @see keyGetValueSize()
	 */
	size_t dataSize;

	/**
	 * The name of the key.
	 * @see keySetName(), keySetName()
	 */
	char * key;

	/**
	 * Size of the name, in bytes, including ending NULL.
	 * @see keyGetName(), keyGetNameSize(), keySetName()
	 */
	size_t keySize;

	/**
	 * Size of the unescaped key name in bytes, including all NULL.
	 * @see keyBaseName(), keyUnescapedName()
	 */
	size_t keyUSize;

	/**
	 * Some control and internal flags.
	 */
	keyflag_t flags;

	/**
	 * In how many keysets the key resists.
	 * keySetName() is only allowed if ksReference is 0.
	 * @see ksPop(), ksAppendKey(), ksAppend()
	 */
	size_t ksReference;

	/**
	 * All the key's meta information.
	 */
	KeySet * meta;
};


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
struct _KeySet
{
	struct _Key ** array; /**<Array which holds the keys */

	size_t size;  /**< Number of keys contained in the KeySet */
	size_t alloc; /**< Allocated size of array */

	struct _Key * cursor; /**< Internal cursor */
	size_t current;		  /**< Current position of cursor */

	/**
	 * Some control and internal flags.
	 */
	ksflag_t flags;
};


/**
 * Helper for identifying global plugin positions
 */

typedef enum {
	PREROLLBACK = 0,
	POSTROLLBACK,
	PREGETSTORAGE,
	POSTGETSTORAGE,
	POSTGETCLEANUP,
	PRESETSTORAGE,
	PRESETCLEANUP,
	PRECOMMIT,
	POSTCOMMIT,
	NR_GLOBAL_PLUGINS
} GlobalpluginPositions;

/**
 * The access point to the key database.
 *
 * The structure which holds all information about loaded backends.
 *
 * Its internal private attributes should not be accessed directly.
 *
 * See kdb mount tool to mount new backends.
 *
 * KDB object is defined as:
 * @code
typedef struct _KDB KDB;
 * @endcode
 *
 * @see kdbOpen() and kdbClose() for external use
 * @ingroup backend
 */

struct _KDB
{
	Trie * trie; /*!< The pointer to the trie holding backends.*/

	Split * split; /*!< A list of all mountpoints. It basically has the
			same information than in the trie, but it is not trivial
			to convert from one to the other.*/

	KeySet * modules; /*!< A list of all modules loaded at the moment.*/

	Backend * defaultBackend; /*!< The default backend as fallback when nothing else is found.*/

	Backend * initBackend; /*!< The init backend for bootstrapping.*/

	Plugin * globalPlugins[NR_GLOBAL_PLUGINS];
};


/**
 * Holds all information related to a backend.
 *
 * Since Elektra 0.8 a Backend consists of many plugins.
 * A backend is responsible for everything related to the process
 * of writing out or reading in configuration.
 *
 * So this holds a list of set and get plugins.
 *
 * Backends are put together through the configuration
 * in system/elektra/mountpoints
 *
 * See kdb mount tool to mount new backends.
 *
 * To develop a backend you have first to develop plugins and describe
 * through dependencies how they belong together.
 *
 * @ingroup backend
 */
struct _Backend
{
	Key * mountpoint; /*!< The mountpoint where the backend resides.
	  The keyName() is the point where the backend was mounted.
	  The keyValue() is the name of the backend without pre/postfix, e.g.
	  filesys. */

	Plugin * setplugins[NR_OF_PLUGINS];
	Plugin * getplugins[NR_OF_PLUGINS];
	Plugin * errorplugins[NR_OF_PLUGINS];

	ssize_t specsize;	/*!< The size of the spec key from the previous get.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */
	ssize_t dirsize;	/*!< The size of the dir key from the previous get.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */
	ssize_t usersize;	/*!< The size of the users key from the previous get.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */
	ssize_t systemsize; /*!< The size of the systems key from the previous get.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */

	size_t refcounter; /*!< This refcounter shows how often the backend
	   is used.  Not cascading or default backends have 1 in it.
	   More than three is not possible, because a backend
	   can be only mounted in dir, system and user each once
	   OR only in spec.*/
};

/**
 * Holds all information related to a plugin.
 *
 * Since Elektra 0.8 a Backend consists of many plugins.
 *
 * A plugin should be reusable and only implement a single concern.
 * Plugins which are supplied with Elektra are located below src/plugins.
 * It is no problem that plugins are developed external too.
 *
 * @ingroup backend
 */
struct _Plugin
{
	KeySet * config; /*!< This keyset contains configuration for the plugin.
	 Direct below system/ there is the configuration supplied for the backend.
	 Direct below user/ there is the configuration supplied just for the
	 plugin, which should be of course prefered to the backend configuration.
	 The keys inside contain information like /path which path should be used
	 to write configuration to or /host to which host packets should be send.
	 @see elektraPluginGetConfig() */

	kdbOpenPtr kdbOpen;   /*!< The pointer to kdbOpen_template() of the backend. */
	kdbClosePtr kdbClose; /*!< The pointer to kdbClose_template() of the backend. */

	kdbGetPtr kdbGet;	  /*!< The pointer to kdbGet_template() of the backend. */
	kdbSetPtr kdbSet;	  /*!< The pointer to kdbSet_template() of the backend. */
	kdbErrorPtr kdbError; /*!< The pointer to kdbError_template() of the backend. */

	const char * name; /*!< The name of the module responsible for that plugin. */

	size_t refcounter; /*!< This refcounter shows how often the plugin
	   is used.  Not shared plugins have 1 in it */

	void * data; /*!< This handle can be used for a plugin to store
	 any data its want to. */
};


/**
 * @defgroup internaldatastructs Internal Datastructures
 * @brief Internal used Datastructures
 */


/**
 * @defgroup trie Trie
 * @ingroup internaldatastructs
 *
 * The private trie structure.
 *
 * A trie is a data structure which can handle the longest prefix matching very
 * fast. This is exactly what needs to be done when using kdbGet() and kdbSet()
 * in a hierarchy where backends are mounted - you need the backend mounted
 * closest to the parentKey.
 */
struct _Trie
{
	struct _Trie * children[KDB_MAX_UCHAR]; /*!< The children building up the trie recursively */
	char * text[KDB_MAX_UCHAR];		/*!< Text identifying this node */
	size_t textlen[KDB_MAX_UCHAR];		/*!< Length of the text */
	Backend * value[KDB_MAX_UCHAR];		/*!< Pointer to a backend */
	Backend * empty_value;			/*!< Pointer to a backend for the empty string "" */
};

typedef enum {
	SPLIT_FLAG_SYNC = 1, /*!< KeySet in Split need sync.
		 Is there any key in there which need to be synced?
		 If keys were popped from the Keyset
		 this flag will be set, so that the backend will sync
		 the keys to database.
		 */

	SPLIT_FLAG_CASCADING = 1 << 1 /*!< Do we need relative checks?
			  Is this a cascading backend?
			  */
} splitflag_t;


/** The private split structure.
 *
 * kdbGet() and kdbSet() split keysets. This structure contains arrays for
 * various information needed to process the keysets afterwards.
 */
struct _Split
{
	size_t size;		/*!< Number of keysets */
	size_t alloc;		/*!< How large the arrays are allocated  */
	KeySet ** keysets;		/*!< The keysets */
	Backend ** handles;		/*!< The KDB for the keyset */
	Key ** parents;		/*!< The parentkey for the keyset.
				Is either the mountpoint of the backend
				or "user", "system", "spec" for the split root/cascading backends */
	splitflag_t * syncbits; /*!< Bits for various options, see #splitflag_t for documentation */
};

// clang-format on

/***************************************
 *
 * Not exported functions, for internal use only
 *
 **************************************/

ssize_t keySetRaw (Key * key, const void * newBinary, size_t dataSize);

/*Methods for split keysets */
Split * elektraSplitNew (void);
void elektraSplitDel (Split * keysets);
void elektraSplitResize (Split * ret);
void elektraSplitRemove (Split * split, size_t where);
ssize_t elektraSplitAppend (Split * split, Backend * backend, Key * parentKey, int syncbits);
ssize_t elektraSplitSearchBackend (Split * split, Backend * backend, Key * key);
int elektraSplitSearchRoot (Split * split, Key * parentKey);
int elektraSplitBuildup (Split * split, KDB * handle, Key * parentKey);
void elektraSplitUpdateFileName (Split * split, KDB * handle, Key * key);

/* for kdbOpen() algorithm */
void elektraSplitOpen (Split * split);

/* for kdbGet() algorithm */
int elektraSplitAppoint (Split * split, KDB * handle, KeySet * ks);
int elektraSplitGet (Split * split, Key * warningKey, KDB * handle);
int elektraSplitMerge (Split * split, KeySet * dest);

/* for kdbSet() algorithm */
int elektraSplitCheckSize (Split * split);
int elektraSplitDivide (Split * split, KDB * handle, KeySet * ks);
int elektraSplitSync (Split * split);
void elektraSplitPrepare (Split * split);
int elektraSplitUpdateSize (Split * split);


/*Backend handling*/
Backend * elektraBackendOpen (KeySet * elektra_config, KeySet * modules, Key * errorKey);
Backend * elektraBackendOpenMissing (Key * mountpoint);
Backend * elektraBackendOpenDefault (KeySet * modules, const char * file, Key * errorKey);
Backend * elektraBackendOpenModules (KeySet * modules, Key * errorKey);
Backend * elektraBackendOpenVersion (Key * errorKey);
int elektraBackendClose (Backend * backend, Key * errorKey);

int elektraBackendUpdateSize (Backend * backend, Key * parent, int size);

/*Plugin handling*/
int elektraProcessPlugin (Key * cur, int * pluginNumber, char ** pluginName, char ** referenceName, Key * errorKey);
int elektraProcessPlugins (Plugin ** plugins, KeySet * modules, KeySet * referencePlugins, KeySet * config, KeySet * systemConfig,
			   Key * errorKey);

Plugin * elektraPluginOpen (const char * backendname, KeySet * modules, KeySet * config, Key * errorKey);
int elektraPluginClose (Plugin * handle, Key * errorKey);
Plugin * elektraPluginMissing (void);
Plugin * elektraPluginVersion (void);

/*Trie handling*/
Trie * elektraTrieOpen (KeySet * config, KeySet * modules, Key * errorKey);
int elektraTrieClose (Trie * trie, Key * errorKey);
Backend * elektraTrieLookup (Trie * trie, const Key * key);
Trie * elektraTrieInsert (Trie * trie, const char * name, Backend * value);

/*Mounting handling */
int elektraMountOpen (KDB * kdb, KeySet * config, KeySet * modules, Key * errorKey);
int elektraMountDefault (KDB * kdb, KeySet * modules, int inFallback, Key * errorKey);
int elektraMountModules (KDB * kdb, KeySet * modules, Key * errorKey);
int elektraMountVersion (KDB * kdb, Key * errorKey);
int elektraMountGlobals (KDB * kdb, KeySet * keys, KeySet * modules, Key * errorKey);
int elektraMountBackend (KDB * kdb, Backend * backend, Key * errorKey);

Key * elektraMountGetMountpoint (KDB * handle, const Key * where);
Backend * elektraMountGetBackend (KDB * handle, const Key * key);

int keyInit (Key * key);
void keyVInit (Key * key, const char * keyname, va_list ap);

int keyClearSync (Key * key);

/*Private helper for keyset*/
int ksInit (KeySet * ks);
int ksClose (KeySet * ks);

int ksResize (KeySet * ks, size_t size);
size_t ksGetAlloc (const KeySet * ks);
KeySet * ksDeepDup (const KeySet * source);

Key * elektraKsPrev (KeySet * ks);
Key * elektraKsPopAtCursor (KeySet * ks, cursor_t pos);

int elektraKeyLock (Key * key, enum elektraLockOptions what);

ssize_t ksSearchInternal (const KeySet * ks, const Key * toAppend);

/*Used for internal memcpy/memmove*/
ssize_t elektraMemcpy (Key ** array1, Key ** array2, size_t size);
ssize_t elektraMemmove (Key ** array1, Key ** array2, size_t size);

char * elektraStrNDup (const char * s, size_t l);
ssize_t elektraFinalizeName (Key * key);
ssize_t elektraFinalizeEmptyName (Key * key);

char * elektraEscapeKeyNamePart (const char * source, char * dest);

size_t elektraUnescapeKeyName (const char * source, char * dest);
int elektraUnescapeKeyNamePartBegin (const char * source, size_t size, char ** dest);
char * elektraUnescapeKeyNamePart (const char * source, size_t size, char * dest);

int elektraValidateKeyName (const char * name, size_t size);

/*Internally used for array handling*/
int elektraArrayValidateName (const Key * key);
int elektraReadArrayNumber (const char * baseName, kdb_long_long_t * oldIndex);

KeySet * elektraRenameKeys (KeySet * config, const char * name);

/* Name Manipulation Methods */
ssize_t keyGetParentName (const Key * key, char * returned, size_t maxSize);
ssize_t keyGetParentNameSize (const Key * key);


/* Conveniences Methods for Making Tests */

int keyIsSpec (const Key * key);
int keyIsProc (const Key * key);
int keyIsDir (const Key * key);
int keyIsSystem (const Key * key);
int keyIsUser (const Key * key);

int keyNameIsSpec (const char * keyname);
int keyNameIsProc (const char * keyname);
int keyNameIsDir (const char * keyname);
int keyNameIsSystem (const char * keyname);
int keyNameIsUser (const char * keyname);

/** Test a bit. @see set_bit(), clear_bit() */
#define test_bit(var, bit) ((var) & (bit))
/** Set a bit. @see clear_bit() */
#define set_bit(var, bit) ((var) |= (bit))
/** Clear a bit. @see set_bit() */
#define clear_bit(var, bit) ((var) &= ~(bit))

#ifdef __cplusplus
}
}
#endif

#endif /* KDBPRIVATE_H */

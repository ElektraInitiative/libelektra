/**
 * @file
 *
 * @brief Private declarations.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBPRIVATE_H
#define KDBPRIVATE_H

#include <elektra.h>
#include <elektra/error.h>
#include <kdb.h>
#include <kdbextension.h>
#include <kdbhelper.h>
#include <kdbio.h>
#include <kdbmacros.h>
#include <kdbnotificationinternal.h>
#include <kdbplugin.h>
#include <kdbtypes.h>
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
#include <kdbopmphm.h>
#include <kdbopmphmpredictor.h>
#endif
#include <kdbglobal.h>

#include <limits.h>

/** The minimal allocation size of a keyset inclusive
	NULL byte. ksGetAlloc() will return one less because
	it says how much can actually be stored.*/
#define KEYSET_SIZE 16

#define NR_OF_GET_PLUGINS 4
#define NR_OF_SET_PLUGINS 6
#define NR_OF_ERROR_PLUGINS 3

#define GET_GETRESOLVER 0
#define GET_PREGETSTORAGE 1
#define GET_GETSTORAGE 2
#define GET_POSTGETSTORAGE 3

#define SET_SETRESOLVER 0
#define SET_PRESETSTORAGE 1
#define SET_SETSTORAGE 2
#define SET_PRECOMMIT 3
#define SET_COMMIT 4
#define SET_POSTCOMMIT 5

#define ERROR_PREROLLBACK 0
#define ERROR_ROLLBACK 1
#define ERROR_POSTROLLBACK 2

/** Trie optimization */
#define APPROXIMATE_NR_OF_BACKENDS 16

/** The maximum value of unsigned char+1, needed
 *  for iteration over trie children/values:
 *
 *  for (i=0; i<KDB_MAX_UCHAR; ++i)
 * */
#define KDB_MAX_UCHAR (UCHAR_MAX + 1)


/**The maximum of how many characters an integer
  needs as decimal number.*/
#define MAX_LEN_INT 31

/**Backend mounting information.
 *
 * This key directory tells you where each backend is mounted
 * to which mountpoint. */
#define KDB_SYSTEM_ELEKTRA "system:/elektra"

/** All keys below this are used for cache metadata in the global keyset */
#define KDB_CACHE_PREFIX "system:/elektra/cache"


#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

#if 1 == 0
typedef struct _Trie Trie;
typedef struct _Split Split;
#endif


/* These define the type for pointers to all the kdb functions */
typedef int (*kdbOpenPtr) (Plugin *, ElektraKey * errorKey);
typedef int (*kdbClosePtr) (Plugin *, ElektraKey * errorKey);

typedef int (*kdbInitPtr) (Plugin * handle, ElektraKeyset * definition, ElektraKey * parentKey);
typedef int (*kdbGetPtr) (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);
typedef int (*kdbSetPtr) (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);
typedef int (*kdbErrorPtr) (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);
typedef int (*kdbCommitPtr) (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);

typedef Plugin * (*OpenMapper) (const char *, const char *, ElektraKeyset *);
typedef int (*CloseMapper) (Plugin *);


/*****************
 * Key Flags
 *****************/

enum
{
	ELEKTRA_KEY_EMPTY_NAME = 1 << 22
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
	ELEKTRA_KEY_FLAG_SYNC = 1,	  /*!<
			Key need sync.
			If name, value or metadata
			are changed this flag will be set, so that the backend will sync
			the key to database.*/
	ELEKTRA_KEY_FLAG_RO_NAME = 1 << 1,	/*!<
			 Read only flag for name.
			 Key name is read only and not allowed
			 to be changed. All attempts to change the name
			 will lead to an error.
			 Needed for metakeys and keys that are in a data
			 structure that depends on name ordering.*/
	ELEKTRA_KEY_FLAG_RO_VALUE = 1 << 2, /*!<
			 Read only flag for value.
			 Key value is read only and not allowed
			 to be changed. All attempts to change the value
			 will lead to an error.
			 Needed for metakeys*/
	ELEKTRA_KEY_FLAG_RO_META = 1 << 3,	/*!<
			 Read only flag for meta.
			 Key meta is read only and not allowed
			 to be changed. All attempts to change the value
			 will lead to an error.
			 Needed for metakeys.*/
	ELEKTRA_KEY_FLAG_MMAP_STRUCT = 1 << 4,	/*!<
			 Key struct lies inside a mmap region.
			 This flag is set for Keys inside a mapped region.
			 It prevents erroneous free() calls on these keys. */
	ELEKTRA_KEY_FLAG_MMAP_KEY = 1 << 5,	/*!<
			 Key name lies inside a mmap region.
			 This flag is set once a Key name has been moved to a mapped region,
			 and is removed if the name moves out of the mapped region.
			 It prevents erroneous free() calls on these keys. */
	ELEKTRA_KEY_FLAG_MMAP_DATA = 1 << 6	/*!<
			 Key value lies inside a mmap region.
			 This flag is set once a Key value has been moved to a mapped region,
			 and is removed if the value moves out of the mapped region.
			 It prevents erroneous free() calls on these keys. */
} keyflag_t;


/**
 * Advanced KS Flags.
 *
 * Store a synchronizer state so that the Elektra knows if something
 * has changed or not.
 *
 * @ingroup backend
 */
typedef enum {
	ELEKTRA_KS_FLAG_SYNC = 1 /*!<
		 KeySet need sync.
		 If keys were popped from the Keyset
		 this flag will be set, so that the backend will sync
		 the keys to database.*/
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	,ELEKTRA_KS_FLAG_NAME_CHANGE = 1 << 1 /*!<
		 The OPMPHM needs to be rebuild.
		 Every Key add, Key removal or Key name change operation
		 sets this flag.*/
#endif
	,ELEKTRA_KS_FLAG_MMAP_STRUCT = 1 << 2	/*!<
		 KeySet struct lies inside a mmap region.
		 This flag is set for KeySets inside a mapped region.
		 It prevents erroneous free() calls on these KeySets. */
	,ELEKTRA_KS_FLAG_MMAP_ARRAY = 1 << 3	/*!<
		 Array of the KeySet lies inside a mmap region.
		 This flag is set for KeySets where the array is in a mapped region,
		 and is removed if the array is moved out from the mapped region.
		 It prevents erroneous free() calls on these arrays. */
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
	 * The canonical (escaped) name of the key.
	 * @see keyGetName(), keySetName()
	 */
	char * key;

	/**
	 * Size of the name, in bytes, including ending NULL.
	 * @see keyGetName(), keyGetNameSize(), keySetName()
	 */
	size_t keySize;

	/**
	 * The unescaped name of the key.
	 * Note: This is NOT a standard null-terminated string.
	 * @see keyGetName(), keySetName()
	 */
	char * ukey;

	/**
	 * Size of the unescaped key name in bytes, including all NULL.
	 * @see keyBaseName(), keyUnescapedName()
	 */
	size_t keyUSize;

	/**
	 * All the key's meta information.
	 */
	ElektraKeyset * meta;

	/**
	 * Some control and internal flags.
	 */
	keyflag_t flags;

	/**
	 * Reference counter
	 */
	uint16_t refs;

	/**
	 * Reserved for future use
	 */
	uint16_t reserved;
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

	uint16_t refs; /**< Reference counter */

	uint16_t reserved; /**< Reserved for future use */

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	/**
	 * The Order Preserving Minimal Perfect Hash Map.
	 */
	Opmphm * opmphm;
	/**
	 * The Order Preserving Minimal Perfect Hash Map Predictor.
	 */
	OpmphmPredictor * opmphmPredictor;
#endif
};


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
#if 1 == 0
	Trie * trie; /*!< The pointer to the trie holding backends.*/

	Split * split; /*!< A list of all mountpoints. It basically has the
			same information than in the trie, but it is not trivial
			to convert from one to the other.*/
#endif

	// TODO (kodebach) [Q]: use global KeySet for modules?
	ElektraKeyset * modules; /*!< A list of all modules loaded at the moment.*/

#if 1 == 0
	Plugin * defaultBackend; /*!< The default backend as fallback when nothing else is found.*/

	Plugin * initBackend; /*!< The init backend for bootstrapping.*/
#endif

	ElektraKeyset * global; /*!< This keyset can be used by plugins to pass data through
			the KDB and communicate with other plugins. Plugins shall clean
			up their parts of the global keyset, which they do not need any more.*/

	Plugin * globalPlugins[NR_GLOBAL_POSITIONS][NR_GLOBAL_SUBPOSITIONS];

	ElektraKeyset * backends;
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
	ElektraKeyset * config; /*!< This keyset contains configuration for the plugin.
	 Direct below system:/ there is the configuration supplied for the backend.
	 Direct below user:/ there is the configuration supplied just for the
	 plugin, which should be of course preferred to the backend configuration.
	 The keys inside contain information like /path which path should be used
	 to write configuration to or /host to which host packets should be send.
	 @see elektraPluginGetConfig() */

	kdbOpenPtr kdbOpen;   /*!< The pointer to kdbOpen_template() of the backend. */
	kdbClosePtr kdbClose; /*!< The pointer to kdbClose_template() of the backend. */

	kdbInitPtr kdbInit;	  /*!< The pointer to kdbInit_template() of the backend. */
	kdbGetPtr kdbGet;	  /*!< The pointer to kdbGet_template() of the backend. */
	kdbSetPtr kdbSet;	  /*!< The pointer to kdbSet_template() of the backend. */
	kdbErrorPtr kdbError; /*!< The pointer to kdbError_template() of the backend. */
	kdbCommitPtr kdbCommit; /*!< The pointer to kdbCommit_template() of the backend. */

	const char * name; /*!< The name of the module responsible for that plugin. */

	size_t refcounter; /*!< This refcounter shows how often the plugin
	   is used.  Not shared plugins have 1 in it */

	void * data; /*!< This handle can be used for a plugin to store
	 any data its want to. */

	ElektraKeyset * global; /*!< This keyset can be used by plugins to pass data through
			the KDB and communicate with other plugins. Plugins shall clean
			up their parts of the global keyset, which they do not need any more.*/

	ElektraKeyset * modules; /*!< A list of all currently loaded modules.*/
};

// FIXME (kodebach): document
typedef struct _BackendData
{
	struct _Plugin * backend;
	struct _KeySet * keys;
	struct _KeySet * plugins;
	struct _KeySet * definition;
	size_t getSize;
	bool initialized;
	bool keyNeedsSync;
} BackendData;

#if 1 == 0
/**
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
	Plugin * value[KDB_MAX_UCHAR];		/*!< Pointer to a backend */
	Plugin * empty_value;			/*!< Pointer to a backend for the empty string "" */
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
	ElektraKeyset ** keysets;		/*!< The keysets */
	Plugin ** handles;		/*!< The KDB for the keyset */
	ElektraKey ** parents;		/*!< The parentkey for the keyset.
				Is either the mountpoint of the backend
				or "user", "system", "spec" for the split root/cascading backends */
	splitflag_t * syncbits; /*!< Bits for various options, see #splitflag_t for documentation */

	ssize_t * specsizes;   /*!< The size of the spec key from the previous get for each backend in the split.
	    -1 if still uninitialized.
	    Needed to know if a key was removed from a keyset. */
	ssize_t * dirsizes;    /*!< The size of the dir key from the previous get for each backend in the split.
	    -1 if still uninitialized.
	    Needed to know if a key was removed from a keyset. */
	ssize_t * usersizes;   /*!< The size of the users key from the previous get for each backend in the split.
	    -1 if still uninitialized.
	    Needed to know if a key was removed from a keyset. */
	ssize_t * systemsizes; /*!< The size of the systems key from the previous get for each backend in the split.
		-1 if still uninitialized.
		Needed to know if a key was removed from a keyset. */
};
#endif

// clang-format on

/***************************************
 *
 * Not exported functions, for internal use only
 *
 **************************************/

ssize_t elektraKeySetRaw (ElektraKey * key, const void * newBinary, size_t dataSize);

#if 1 == 0
/*Methods for split keysets */
Split * splitNew (void);
void splitDel (Split * keysets);
void splitRemove (Split * split, size_t where);
ssize_t splitAppend (Split * split, Plugin * backend, ElektraKey * parentKey, int syncbits);
int splitBuildup (Split * split, ElektraKdb * handle, ElektraKey * parentKey);
void splitUpdateFileName (Split * split, ElektraKdb * handle, ElektraKey * key);

/* for kdbGet() algorithm */
int splitAppoint (Split * split, ElektraKdb * handle, ElektraKeyset * ks);
int splitGet (Split * split, ElektraKey * warningKey, ElektraKdb * handle);
int splitMergeBackends (Split * split, ElektraKeyset * dest);
int splitMergeDefault (Split * split, ElektraKeyset * dest);

/* for kdbSet() algorithm */
int splitDivide (Split * split, ElektraKdb * handle, ElektraKeyset * ks);
int splitSync (Split * split);
void splitPrepare (Split * split);
int splitUpdateSize (Split * split);

/* for cache: store/load state to/from global keyset */
void splitCacheStoreState (ElektraKdb * handle, Split * split, ElektraKeyset * global, ElektraKey * parentKey, ElektraKey * initialParent);
int splitCacheCheckState (Split * split, ElektraKeyset * global);
int splitCacheLoadState (Split * split, ElektraKeyset * global);
#endif

ElektraKey * backendsFindParent (ElektraKeyset * backends, const ElektraKey * key);
ElektraKeyset * backendsForParentKey (ElektraKeyset * backends, ElektraKey * parentKey);
bool backendsDivide (ElektraKeyset * backends, const ElektraKeyset * ks);
void backendsMerge (ElektraKeyset * backends, ElektraKeyset * ks);

ElektraKeyset * elektraMountpointsParse (ElektraKeyset * elektraKs, ElektraKeyset * modules, ElektraKeyset * global, ElektraKey * errorKey);

/*Backend handling*/
Plugin * backendOpen (ElektraKeyset * elektra_config, ElektraKeyset * modules, ElektraKeyset * global, ElektraKey * errorKey);
Plugin * backendOpenDefault (ElektraKeyset * modules, ElektraKeyset * global, const char * file, ElektraKey * errorKey);
Plugin * backendOpenModules (ElektraKeyset * modules, ElektraKeyset * global, ElektraKey * errorKey);
Plugin * backendOpenVersion (ElektraKeyset * global, ElektraKeyset * modules, ElektraKey * errorKey);
ElektraKey * backendGetMountpoint (const Plugin * backend);

#if 1 == 0
int backendUpdateSize (Split * split, int index, ElektraKey * parent, int size);
#endif

/*Plugin handling*/
Plugin * elektraPluginOpen (const char * backendname, ElektraKeyset * modules, ElektraKeyset * config, ElektraKey * errorKey);
int elektraPluginClose (Plugin * handle, ElektraKey * errorKey);
int elektraProcessPlugin (ElektraKey * cur, int * pluginNumber, char ** pluginName, char ** referenceName, ElektraKey * errorKey);
size_t elektraPluginGetFunction (Plugin * plugin, const char * name);
Plugin * elektraPluginFindGlobal (ElektraKdb * handle, const char * pluginName);


#if 1 == 0
/*Trie handling*/
int trieClose (Trie * trie, ElektraKey * errorKey);
Plugin * trieLookup (Trie * trie, const ElektraKey * key);
Trie * trieInsert (Trie * trie, const char * name, Plugin * value);
#endif

/*Mounting handling */
int mountOpen (ElektraKdb * kdb, ElektraKeyset * config, ElektraKeyset * modules, ElektraKey * errorKey);
int mountDefault (ElektraKdb * kdb, ElektraKeyset * modules, ElektraKey * errorKey);
int mountModules (ElektraKdb * kdb, ElektraKeyset * modules, ElektraKey * errorKey);
int mountVersion (ElektraKdb * kdb, ElektraKey * errorKey);
int mountGlobals (ElektraKdb * kdb, ElektraKeyset * keys, ElektraKeyset * modules, ElektraKey * errorKey);
int mountBackend (ElektraKdb * kdb, const ElektraKey * mountpoint, Plugin * backend);

const ElektraKey * mountGetMountpoint (ElektraKdb * handle, ElektraKey * where);
Plugin * mountGetBackend (ElektraKdb * handle, ElektraKey * key);

void keyInit (ElektraKey * key);

int elektraKeyClearSync (ElektraKey * key);

int elektraKeyReplacePrefix (ElektraKey * key, const ElektraKey * oldPrefix, const ElektraKey * newPrefix);

/*Private helper for keyset*/
int elektraKeysetInit (ElektraKeyset * ks);
int elektraKeysetClose (ElektraKeyset * ks);

int elektraKeysetResize (ElektraKeyset * ks, size_t size);
size_t elektraKeysetGetAlloc (const ElektraKeyset * ks);
ElektraKeyset * elektraKeysetDeepDup (const ElektraKeyset * source);

ElektraKey * elektraKsPopAtCursor (ElektraKeyset * ks, elektraCursor pos);

/*Used for internal memcpy/memmove*/
ssize_t elektraMemcpy (ElektraKey ** array1, ElektraKey ** array2, size_t size);
ssize_t elektraMemmove (ElektraKey ** array1, ElektraKey ** array2, size_t size);

/*Internally used for array handling*/
int elektraReadArrayNumber (const char * baseName, kdb_long_long_t * oldIndex);


ElektraKeyset * elektraKeysetRenameKeys (ElektraKeyset * config, const char * name);

ssize_t ksRename (ElektraKeyset * ks, const ElektraKey * root, const ElektraKey * newRoot);

elektraCursor elektraKeysetFindHierarchy (const ElektraKeyset * ks, const ElektraKey * root, elektraCursor * end);
ElektraKeyset * ksBelow (const ElektraKeyset * ks, const ElektraKey * root);


/* Conveniences Methods for Making Tests */

int elektraKeyIsSpec (const ElektraKey * key);
int elektraKeyIsProc (const ElektraKey * key);
int elektraKeyIsDir (const ElektraKey * key);
int elektraKeyIsSystem (const ElektraKey * key);
int elektraKeyIsUser (const ElektraKey * key);

elektraNamespace elektraReadNamespace (const char * namespaceStr, size_t len);

bool elektraKeyNameValidate (const char * name, bool isComplete);
void elektraKeyNameCanonicalize (const char * name, char ** canonicalName, size_t * canonicalSizePtr, size_t offset, size_t * usizePtr);
void elektraKeyNameUnescape (const char * name, char * unescapedName);
size_t elektraKeyNameEscapePart (const char * part, char ** escapedPart);

// TODO (kodebaach) [Q]: make public?
int elektraIsArrayPart (const char * namePart);

/* global plugin calls */
int elektraGlobalGet (ElektraKdb * handle, ElektraKeyset * ks, ElektraKey * parentKey, int position, int subPosition);
int elektraGlobalSet (ElektraKdb * handle, ElektraKeyset * ks, ElektraKey * parentKey, int position, int subPosition);
int elektraGlobalError (ElektraKdb * handle, ElektraKeyset * ks, ElektraKey * parentKey, int position, int subPosition);

/** Test a bit. @see set_bit(), clear_bit() */
#define test_bit(var, bit) (((unsigned long long) (var)) & ((unsigned long long) (bit)))
/** Set a bit. @see clear_bit() */
#define set_bit(var, bit) ((var) |= ((unsigned long long) (bit)))
/** Clear a bit. @see set_bit() */
#define clear_bit(var, bit) ((var) &= ~((unsigned long long) (bit)))

#ifdef __cplusplus
}
}

#define ElektraKdb ckdb::KDB
#define ElektraKey ckdb::Key
#define ElektraKeyset ckdb::KeySet
extern "C" {
#endif

struct _Elektra
{
	ElektraKdb * kdb;
	ElektraKey * parentKey;
	ElektraKeyset * config;
	ElektraKeyset * defaults;
	ElektraKey * lookupKey;
	ElektraErrorHandler fatalErrorHandler;
	char * resolvedReference;
	size_t parentKeyLength;
};

struct _ElektraError
{
	char * code;
	char * codeFromKey;
	char * description;
	char * module;
	char * file;
	kdb_long_t line;
	kdb_long_t warningCount;
	kdb_long_t warningAlloc;
	struct _ElektraError ** warnings;
	ElektraKey * errorKey;
};

/* high-level API */
void elektraSaveKey (Elektra * elektra, ElektraKey * key, ElektraError ** error);
void elektraSetLookupKey (Elektra * elektra, const char * name);
void elektraSetArrayLookupKey (Elektra * elektra, const char * name, kdb_long_long_t index);

ElektraError * elektraErrorCreate (const char * code, const char * description, const char * module, const char * file, kdb_long_t line);
void elektraErrorAddWarning (ElektraError * error, ElektraError * warning);
ElektraError * elektraErrorFromKey (ElektraKey * key);

ElektraError * elektraErrorKeyNotFound (const char * keyname);
ElektraError * elektraErrorWrongType (const char * keyname, KDBType expectedType, KDBType actualType);
ElektraError * elektraErrorNullError (const char * function);
ElektraError * elektraErrorEnsureFailed (const char * reason);

#ifdef __cplusplus
}
#undef Key
#undef KeySet
#undef KDB
#endif

#endif /* KDBPRIVATE_H */

/***************************************************************************
      kdbprivate.h  -  Private classes definition

                           -------------------
 *  begin                : Wed 19 May, 2010
 *  copyright            : (C) 2010 by Markus Raab
 *  email                : elektra@markus-raab.org
 ***************************************************************************/

@DISCLAMER@

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef KDBPRIVATE_H
#define KDBPRIVATE_H

#include <kdb.h>
#include <kdbextension.h>
#include <kdbplugin.h>

#include <limits.h>

#include <kdbcap.h>
#include <kdbloader.h>

#ifndef KDB_DB_SYSTEM
/**Below this directory the system configuration (system/) will be searched.*/
#define KDB_DB_SYSTEM            "@KDB_DB_SYSTEM@"
#endif

#ifndef KDB_DB_USER
/**This directory will be concatenated with a prefix which will be searched
 * at runtime inside kdbbGetFullFilename().
 *@see kdbbGetFullFilename
 */
#define KDB_DB_USER              ".kdb"
#endif

#ifndef KDB_DB_HOME
/**This directory will be used as fallback when no other method of
 * kdbbGetFullFilename() works.
 *@see kdbbGetFullFilename
 */
#define KDB_DB_HOME              "/home"
#endif

#ifndef KDB_KEY_MOUNTPOINTS
/**Backend information.
 *
 * This key directory tells you where each backend is mounted
 * to which mountpoint. */
#define KDB_KEY_MOUNTPOINTS      "system/elektra/mountpoints"
#endif

#define KDB_KEY_MOUNTPOINTS_LEN  (sizeof (KDB_KEY_MOUNTPOINTS))

#ifndef KDB_KEY_USERS
/**Users information.
 *
 * This key directory tells you the users existing on the system. */
#define KDB_KEY_USERS            "system/users"
#endif

#define KDB_KEY_USERS_LEN        (sizeof (KDB_KEY_USERS))

#ifndef MAX_UCHAR
#define MAX_UCHAR (UCHAR_MAX+1)
#endif

#ifndef KEYSET_SIZE
/*The minimal allocation size of a keyset inclusive
  NULL byte. ksGetAlloc() will return one less because
  it says how much can actually be stored.*/
#define KEYSET_SIZE 16
#endif

#ifndef APPROXIMATE_NR_OF_BACKENDS
#define APPROXIMATE_NR_OF_BACKENDS 16
#endif

#ifndef ESCAPE_CHAR
#define ESCAPE_CHAR '\\'
#endif

/**BUFFER_SIZE can be used as value for any I/O buffer
 * on files.
 *
 * It may be used for optimization on various
 * systems.*/
#ifndef BUFFER_SIZE
#define BUFFER_SIZE 256
#endif

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


typedef struct _Trie	Trie;
typedef struct _Split	Split;
typedef struct _Backend	Backend;

/* These define the type for pointers to all the kdb functions */
typedef int  (*kdbOpenPtr)(Plugin *);
typedef int  (*kdbClosePtr)(Plugin *);

typedef ssize_t  (*kdbGetPtr)(Plugin *handle, KeySet *returned, const Key *parentKey);
typedef ssize_t  (*kdbSetPtr)(Plugin *handle, KeySet *returned, const Key *parentKey);


typedef Backend* (*OpenMapper)(const char *,const char *,KeySet *);
typedef int (*CloseMapper)(Backend *);



/*****************
 * Key Flags
 *****************/

/**
 * Key Flags.
 *
 * Store a synchronizer state so that the Elektra knows if something
 * has changed or not.
 *
 * @ingroup backend
 */
typedef enum
{
	KEY_FLAG_SYNC=1,	/*!<
		Key need sync.
		If name or value
		are changed this flag will be set, so that the backend will sync
		the key to database.*/
	KEY_FLAG_META=1<<2,	/*!<
		Key meta need sync.
		TODO: not used currently
		If meta
		is changed this flag will be set, so that the backend will sync
		the key to database.*/
	KEY_FLAG_RO=1<<3	/*!<
		Read only flag.
		TODO: not used currently
		Key is read only and not allowed
		to be changed. All attempts to change name, value,
		or meta data will be ignored.*/
} keyflag_t;


/**
 * Ks Flags.
 *
 * Store a synchronizer state so that the Elektra knows if something
 * has changed or not.
 *
 * @ingroup backend
 */
typedef enum
{
	KS_FLAG_SYNC=1,	/*!<
		KeySet need sync.
		If keys were popped from the Keyset
		this flag will be set, so that the backend will sync
		the keys to database.*/
	KS_FLAG_RO=1<<3	/*!<
		Read only flag.
		KeySet is read only and not allowed
		TODO: not used currently
		to be changed. All attempts to append or pop keys
		will be ignored.*/
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
struct _Key {
	/**
	 * The value, which is a NULL terminated string or binary.
	 * @see keyString(), keyBinary(),
	 * @see keyGetString(), keyGetBinary(),
	 * @see keySetString(), keySetBinary()
	 */
	union {char* c; void * v;} data;

	/**
	 * Size of the value, in bytes, including ending NULL.
	 * @see keyGetCommentSize(), keySetComment(), keyGetComment()
	 */
	size_t         dataSize;

	/**
	 * The name of the key.
	 * @see keySetName(), keySetName()
	 */
	char *         key;

	/**
	 * Size of the name, in bytes, including ending NULL.
	 * @see keyGetName(), keyGetNameSize(), keySetName()
	 */
	size_t         keySize;

	/**
	 * Some control and internal flags.
	 */
	keyflag_t      flags;

	/**
	 * In how many keysets the key resists.
	 * keySetName() is only allowed if ksReference is 0.
	 * @see ksPop(), ksAppendKey(), ksAppend()
	 */
	size_t        ksReference;

	/**
	 * All the key's meta information.
	 */
	KeySet *      meta;
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
struct _KeySet {
	struct _Key **array;	/**<Array which holds the keys */

	size_t        size;	/**< Number of keys contained in the KeySet */
	size_t        alloc;	/**< Allocated size of array */

	struct _Key  *cursor;	/**< Internal cursor */
	size_t        current;	/**< Current position of cursor */

	/**
	 * Some control and internal flags.
	 */
	ksflag_t      flags;
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
struct _KDB {
	Trie *trie;		/*!< The pointer to the trie holding backends.
		@see kdbhGetTrie() */


	/* TODO: remove everything below this point */
	kdbLibHandle dlHandle;	/*!< The pointer to the datastructure to load a new backend. */

	Key *mountpoint;	/*!< The mountpoint where the backend resides.
		The keyName() is the point where the backend was mounted.
		The keyValue() is the name of the backend without pre/postfix, e.g.
		filesys. */
	KeySet *config;		/*!< This keyset contains configuration for the backend.
		Don't care about the absolute path, it may change when dynamically
		kdbMount() or because of another name under system/elektra/mountpoints. 
		The keys inside contain information like /path which path should be used
		to write configuration to or /host to which host packets should be send.
		@see kdbhGetConfig() */

	void *backendData;	/*!< A general pointer for any data backend needs.
		Thus backends are not allowed to have global variables for thread saftey,
		they must use this pointer.
		@see kdbhGetBackendData() */

	KDBCap *capability;	/*!< The capabilites this backend declares to have.
		@see kdbhGetCapability() */

	kdbOpenPtr kdbOpen;	/*!< The pointer to kdbOpen_template() of the backend. */
	kdbClosePtr kdbClose;	/*!< The pointer to kdbClose_template() of the backend. */

	kdbGetPtr kdbGet;	/*!< The pointer to kdbGet_template() of the backend. */
	kdbSetPtr kdbSet;	/*!< The pointer to kdbSet_template() of the backend. */
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
struct _Backend {
	Key *mountpoint;	/*!< The mountpoint where the backend resides.
		The keyName() is the point where the backend was mounted.
		The keyValue() is the name of the backend without pre/postfix, e.g.
		filesys. */

	Plugin *setplugins[10];
	Plugin *getplugins[10];
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
 * TODO: guides how to develop plugins
 *
 * @ingroup backend
 */
struct _Plugin {
	KeySet *config;		/*!< This keyset contains configuration for the plugin.
		Direct below system/ there is the configuration supplied for the backend.
		Direct below user/ there is the configuration supplied just for the
		plugin, which should be of course prefered to the backend configuration.
		The keys inside contain information like /path which path should be used
		to write configuration to or /host to which host packets should be send.
		@see kdbhGetConfig() */

	void *data;	/*!< A general pointer for any plugins needs.
		Thus backends are not allowed to have global variables for thread safety,
		they must use this pointer.
		@note Every information you need between kdbOpen(), kdbClose(), kdbGet()
		      and kdbSet() *must* be stored here.
		@see kdbhGetBackendData() */

	kdbOpenPtr kdbOpen;	/*!< The pointer to kdbOpen_template() of the backend. */
	kdbClosePtr kdbClose;	/*!< The pointer to kdbClose_template() of the backend. */

	kdbGetPtr kdbGet;	/*!< The pointer to kdbGet_template() of the backend. */
	kdbSetPtr kdbSet;	/*!< The pointer to kdbSet_template() of the backend. */

	kdbLibHandle dlHandle;	/*!< The pointer to the datastructure to load a new backend. */
};


/** The private trie structure.
 *
 * A trie is a data structure which can handle the longest prefix matching very
 * fast. This is exactly what needs to be done when using kdbGet() and kdbSet()
 * in a hierachy where backends are mounted - you need the backend mounted
 * closest to the parentKey.
 */
struct _Trie {
	struct _Trie *children[MAX_UCHAR];/*!<  */
	char *text[MAX_UCHAR];		/*!<  */
	unsigned int textlen[MAX_UCHAR];/*!<  */
	void *value[MAX_UCHAR];		/*!<  */
	void *empty_value;		/*!< value for the empty string "" */
};


/** The private split structure.
 *
 * kdbSet() splits keysets. This structure contains arrays for
 * various information needed to process the keysets afterwards.
 */
struct _Split {
	size_t no;		/*!< Number of keysets */
	size_t alloc;		/*!< How large the arrays are allocated  */
	KeySet **keysets;	/*!< The keysets */
	KDB **handles;	/*!< The KDB for the keyset */
	Key **parents;		/*!< The parentkey for the keyset */
	int *syncbits;		/*!< Is there any key in there which need to be synced? */
	int *belowparents;	/*!< Is there any key in there which is below the parent? */
};


#ifdef __cplusplus
extern "C" {
#endif


/***************************************
 *
 * Not exported functions, for internal use only
 *
 **************************************/

ssize_t keySetRaw(Key *key, const void *newBinary, size_t dataSize);

char *keyNameGetOneLevel(const char *keyname, size_t *size);

KDB *kdbGetBackend(KDB *handle, const Key *key);

/*Methods for trie*/
int kdbCreateTrie(KDB *handle, KeySet *ks, OpenMapper mapper);
int kdbDelTrie(Trie *trie,CloseMapper close_backend);

Trie *createTrie(KeySet *ks, OpenMapper mapper);
Trie *delete_trie(Trie *trie, char *name, CloseMapper close_mapper);
Trie *insert_trie(Trie *trie, const char *name, const void *value);

Trie *kdbhGetTrie(const KDB *handle);
void kdbhSetTrie(KDB *handle, Trie *trie);

/*Methods for splitted keysets */
void free_splitted_keysets(Split *keysets);
void init_splitted_keysets(Split *ret);
void resize_splitted_keysets(Split *ret);
Split *split_keyset(KDB *handle, KeySet *ks,
	Key *parentKey, unsigned long options);

/*Internal helpers*/

size_t kdbiMemcpy (Key** array1, Key** array2, size_t size);
size_t kdbiMemmove (Key** array1, Key** array2, size_t size);
void *kdbiMalloc (size_t size);
void  kdbiFree (void *ptr);
char *kdbiStrDup (const char *s);
char *kdbiStrNDup (const char *s, size_t l);
int kdbiRealloc(void **buffer, size_t size);

int kdbiStrCmp (const char *s1, const char *s2);
int kdbiStrCaseCmp (const char *s1, const char *s2);
size_t kdbiStrLen(const char *s);

/*TODO remove those Helpers*/
ssize_t kdbbEncode(void *kdbbDecoded, size_t size, char *returned);
ssize_t kdbbDecode(char *kdbbEncoded, void *returned);

int kdbbNeedsUTF8Conversion(void);
int kdbbUTF8Engine(int direction, char **string, size_t *inputByteSize);

int kdbbEncodeChar(char c, char *buffer, size_t bufSize);
int kdbbDecodeChar(const char *from, char *into);

int kdbbFilenameToKeyName(const char *string, char *buffer, int bufSize);
ssize_t kdbbGetFullKeyName (KDB *handle, const char *forFilename, const Key *parentKey, Key *returned);
int kdbbKeyNameToRelativeFilename(const char *string, char *buffer, size_t bufSize);
ssize_t kdbbKeyCalcRelativeFilename(const Key *key,char *relativeFilename,size_t maxSize);
ssize_t kdbbGetFullFilename(KDB *handle, const Key *forKey,char *returned,size_t maxSize);

/*TODO Old Stuff*/
KDB* kdbOpenBackend(const char *backendname, const char *mountpoint, KeySet *config);
int kdbCloseBackend(KDB *handle);

/*Backend handling*/
Backend* backendOpen(KeySet *elektra_config);
int backendClose(Backend *backend);

/*Plugin handling*/
int renameConfig(KeySet *config);
int processPlugins(Plugin **plugins, KeySet *config, KeySet *systemConfig);

Plugin* pluginOpen(const char *backendname, KeySet *config);
int pluginClose(Plugin *handle);

/*Private helper for keys*/
int keyInit(Key *key);
int keyClose(Key *key);

int ksInit(KeySet *ks);
int ksClose(KeySet *ks);

/** Test a bit. @see set_bit(), clear_bit() */
#define test_bit(var,bit)            ((var) &   (bit))
/** Set a bit. @see clear_bit() */
#define set_bit(var,bit)             ((var) |=  (bit))
/** Clear a bit. @see set_bit() */
#define clear_bit(var,bit)           ((var) &= ~(bit))

#ifdef __cplusplus
}
#endif

#endif /* KDBPRIVATE_H */

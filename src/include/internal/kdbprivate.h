/**
 * @file
 *
 * @brief Private declarations.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBPRIVATE_H
#define KDBPRIVATE_H

#pragma region includes

#include <elektra/config.h>
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/core/namespace.h>
#include <elektra/ease/meta.h>
#include <elektra/highlevel.h>
#include <elektra/highlevel/errors.h>
#include <elektra/io/api.h>
#include <elektra/kdb/kdb.h>
#include <elektra/plugin/plugin.h>
#include <elektra/type/types.h>
#include <internal/notifications.h>
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
#include <internal/core/opmphm.h>
#include <internal/core/opmphmpredictor.h>
#endif
#include <internal/utility/alloc.h>

#include <limits.h>

#pragma endregion

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

#pragma endregion

#pragma region core /key

/**
 * The private copy-on-write key data structure.
 *
 * Its internal private attributes should not be accessed directly by regular
 * programs.
 *
 */
struct _KeyData
{
	/**
	 * The value, which is a NULL terminated string or binary.
	 * @see keyString(), keyBinary(),
	 * @see keyGetString(), keyGetBinary(),
	 * @see keySetString(), keySetBinary()
	 */
	union
	{
		char * c;
		void * v;
	} data;

	/**
	 * Size of the value, in bytes, including ending NULL.
	 * @see keyGetValueSize()
	 */
	size_t dataSize;

	/**
	 * Reference counter
	 */
	uint16_t refs;

	/**
	 * Is this structure and its data stored in an mmap()ed memory area?
	 */
	bool isInMmap : 1;

	/**
	 * Bitfield reserved for future use.
	 * Decrease size when adding new flags.
	 */
	int : 15;
};

/**
 * The private copy-on-write keyname structure.
 *
 * Its internal private attributes should not be accessed directly by regular
 * programs.
 *
 */
struct _KeyName
{
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
	 * Reference counter
	 */
	uint16_t refs;

	/**
	 * Is this structure and its data stored in an mmap()ed memory area?
	 */
	bool isInMmap : 1;

	/**
	 * Bitfield reserved for future use.
	 * Decrease size when adding new flags.
	 */
	int : 15;
};

// private methods for COW keys
struct _KeyName * keyNameNew (void);
struct _KeyName * keyNameCopy (struct _KeyName * source);
uint16_t keyNameRefInc (struct _KeyName * keyname);
uint16_t keyNameRefDec (struct _KeyName * keyname);
uint16_t keyNameRefDecAndDel (struct _KeyName * keyname);
void keyNameDel (struct _KeyName * keyname);

void keyDetachKeyName (Key * key);

struct _KeyData * keyDataNew (void);
uint16_t keyDataRefInc (struct _KeyData * keydata);
uint16_t keyDataRefDec (struct _KeyData * keydata);
uint16_t keyDataRefDecAndDel (struct _KeyData * keydata);
void keyDataDel (struct _KeyData * keydata);

static inline bool isKeyNameInMmap (const struct _KeyName * keyname)
{
	return keyname->isInMmap;
}

static inline void setKeyNameIsInMmap (struct _KeyName * keyname, bool isInMmap)
{
	keyname->isInMmap = isInMmap;
}

static inline bool isKeyDataInMmap (const struct _KeyData * keydata)
{
	return keydata->isInMmap;
}

static inline void setKeyDataIsInMmap (struct _KeyData * keydata, bool isInMmap)
{
	keydata->isInMmap = isInMmap;
}

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
	 * Copy-on-write structure for the key data
	 */
	struct _KeyData * keyData;

	/**
	 * Copy-on-write structure for the key name
	 */
	struct _KeyName * keyName;

	/**
	 * All the key's meta information.
	 */
	KeySet * meta;

	/**
	 * Reference counter
	 */
	uint16_t refs;

	/**
	 * Is this structure stored in an mmap()ed memory area?
	 */
	bool isInMmap : 1;

	/**
	 * Read only flag for name.
	 * Key name is read only and not allowed to be changed.
	 * All attempts to change the name will lead to an error.
	 * Needed for metakeys and keys that are in a data structure that depends on name ordering.
	 */
	bool hasReadOnlyName : 1;

	/**
	 * Read only flag for value.
	 * Key value is read only and not allowed  to be changed.
	 * All attempts to change the value will lead to an error.
	 * Needed for metakeys
	 */
	bool hasReadOnlyValue : 1;

	/**
	 * Read only flag for meta.
	 * Key meta is read only and not allowed to be changed.
	 * All attempts to change the value will lead to an error.
	 * Needed for metakeys.
	 */
	bool hasReadOnlyMeta : 1;

	/**
	 * Bitfield reserved for future use.
	 * Decrease size when adding new flags.
	 */
	int : 11;
};

/*Private helper for key*/
ssize_t keySetRaw (Key * key, const void * newBinary, size_t dataSize);
void keyInit (Key * key);
int keyReplacePrefix (Key * key, const Key * oldPrefix, const Key * newPrefix);

/* Conveniences Methods for Making Tests */

int keyIsSpec (const Key * key);
int keyIsProc (const Key * key);
int keyIsDir (const Key * key);
int keyIsSystem (const Key * key);
int keyIsUser (const Key * key);

/* Name handling */
bool elektraKeyNameValidate (const char * name, bool isComplete);
void elektraKeyNameCanonicalize (const char * name, char ** canonicalName, size_t * canonicalSizePtr, size_t offset, size_t * usizePtr);
void elektraKeyNameUnescape (const char * name, char * unescapedName);
size_t elektraKeyNameEscapePart (const char * part, char ** escapedPart);

// TODO (kodebaach) [Q]: make public?
int elektraIsArrayPart (const char * namePart);

static inline KeySet * keyMetaNoAlloc (const Key * key)
{
	if (!key) return NULL;
	return key->meta;
}
#pragma endregion

#pragma region core /keyset

struct _KeySetData
{
	struct _Key ** array; /**<Array which holds the keys */

	size_t size;  /**< Number of keys contained in the KeySet */
	size_t alloc; /**< Allocated size of array */

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

	uint16_t refs; /**< Reference counter */

	/**
	 * Is this structure and its data stored in an mmap()ed memory area?
	 */
	bool isInMmap : 1;

	/**
	 * Whether opmphm needs to be rebuilt
	 */
	bool isOpmphmInvalid : 1;

	/**
	 * Bitfield reserved for future use.
	 * Decrease size when adding new flags.
	 */
	int : 14;
};

// COW methods for keyset

struct _KeySetData * keySetDataNew (void);
uint16_t keySetDataRefInc (struct _KeySetData * keysetdata);
uint16_t keySetDataRefDec (struct _KeySetData * keysetdata);
uint16_t keySetDataRefDecAndDel (struct _KeySetData * keysetdata);
void keySetDataDel (struct _KeySetData * keysetdata);

void keySetDetachData (KeySet * keyset);

static inline bool isKeySetDataInMmap (const struct _KeySetData * keysetdata)
{
	return keysetdata->isInMmap;
}

static inline void setKeySetDataIsInMmap (struct _KeySetData * keysetdata, bool isInMmap)
{
	keysetdata->isInMmap = isInMmap;
}


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
	/**
	 * Copy-on-write data
	 */
	struct _KeySetData * data;

	struct _Key * cursor; /**< Internal cursor */
	size_t current;	      /**< Current position of cursor */

	uint16_t refs; /**< Reference counter */

	/**
	 * Is this structure stored in an mmap()ed memory area?
	 */
	bool isInMmap : 1;

	/**
	 * KeySet need sync.
	 * If keys were popped from the Keyset this flag will be set,
	 * so that the backend will sync the keys to database.
	 */
	bool needsSync : 1;

	/**
	 * Bitfield reserved for future use.
	 * Decrease size when adding new flags.
	 */
	int : 14;
};


/*Private helper for keyset*/
int ksInit (KeySet * ks);
int ksClose (KeySet * ks);

int ksResize (KeySet * ks, size_t size);
size_t ksGetAlloc (const KeySet * ks);
KeySet * ksDeepDup (const KeySet * source);

Key * elektraKsPopAtCursor (KeySet * ks, elektraCursor pos);

KeySet * ksRenameKeys (KeySet * config, const char * name);

ssize_t ksRename (KeySet * ks, const Key * root, const Key * newRoot);

elektraCursor ksFindHierarchy (const KeySet * ks, const Key * root, elektraCursor * end);
KeySet * ksBelow (const KeySet * ks, const Key * root);

#pragma endregion

#ifdef __cplusplus
}
}
#endif

#endif /* KDBPRIVATE_H */

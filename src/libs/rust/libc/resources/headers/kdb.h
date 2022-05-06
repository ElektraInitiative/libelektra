#include <stdint.h>

enum {
	ELEKTRA_KEY_KEY_VALUE=1<<1,
	ELEKTRA_KEY_KEY_FLAGS=3,
	ELEKTRA_KEY_KEY_BINARY=1<<4,
	ELEKTRA_KEY_KEY_SIZE=1<<11,
	ELEKTRA_KEY_KEY_FUNC=1<<12,
	ELEKTRA_KEY_KEY_META=1<<15,
	ELEKTRA_KEY_KEY_NULL=1<<16,
	ELEKTRA_KEY_KEY_LOCK_NAME=1<<17,
	ELEKTRA_KEY_KEY_LOCK_VALUE=1<<18,
	ELEKTRA_KEY_KEY_LOCK_META=1<<19,
};

typedef int elektraLockFlags;
typedef int elektraKeyFlags;

enum {
	ELEKTRA_KEY_CP_NAME   = 1<<0,
	ELEKTRA_KEY_CP_STRING = 1<<1,
	ELEKTRA_KEY_CP_VALUE  = 1<<2,
	ELEKTRA_KEY_CP_META   = 1<<3,
	ELEKTRA_KEY_CP_ALL    = ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE | ELEKTRA_KEY_CP_META,
};
typedef unsigned int elektraCopyFlags;

enum {
	ELEKTRA_NS_NONE=0,
	ELEKTRA_NS_CASCADING=1,
	ELEKTRA_NS_META=2,
	ELEKTRA_NS_SPEC=3,
	ELEKTRA_NS_PROC=4,
	ELEKTRA_NS_DIR=5,
	ELEKTRA_NS_USER=6,
	ELEKTRA_NS_SYSTEM=7,
	ELEKTRA_NS_DEFAULT=8,
};
typedef int elektraNamespace;

static const elektraNamespace ELEKTRA_NS_FIRST = ELEKTRA_NS_META;
static const elektraNamespace ELEKTRA_NS_LAST = ELEKTRA_NS_DEFAULT;

typedef ssize_t elektraCursor;

typedef struct _KDB	ElektraKDB;
typedef struct _Key	ElektraKey;
typedef struct _KeySet	ElektraKeySet;

/**************************************
 *
 * KDB methods
 *
 **************************************/

ElektraKDB * elektraKdbOpen (const ElektraKeySet * contract, ElektraKey * parentKey);

int elektraKdbClose (ElektraKDB * handle, ElektraKey * errorKey);

int elektraKdbGet (ElektraKDB * handle, ElektraKeySet * ks, ElektraKey * parentKey);
int elektraKdbSet (ElektraKDB * handle, ElektraKeySet * ks, ElektraKey * parentKey);

/**************************************
 *
 * Key methods
 *
 **************************************/

/* Basic Methods */
ElektraKey * elektraKeyNew (const char * keyname, ...);
ElektraKey * elektraKeyVNew (const char * keyname, va_list ap);

ElektraKey * elektraKeyCopy (ElektraKey * dest, const ElektraKey * source, elektraCopyFlags flags);

int elektraKeyClear (ElektraKey * key);
int elektraKeyDel (ElektraKey * key);

uint16_t elektraKeyIncRef (ElektraKey * key);
uint16_t elektraKeyDecRef (ElektraKey * key);
uint16_t elektraKeyGetRef (const ElektraKey * key);

/* Metadata */
ElektraKeySet * elektraKeyMeta (ElektraKey * key);
int elektraKeySetMeta (ElektraKey * key, ElektraKeySet * meta);

/* Relations */
int elektraKeyCompareName (const ElektraKey * k1, const ElektraKey * k2);

int elektraKeyIsBelow (const ElektraKey * key, const ElektraKey * check);
int elektraKeyIsBelowOrSame (const ElektraKey * key, const ElektraKey * check);
int elektraKeyIsDirectlyBelow (const ElektraKey * key, const ElektraKey * check);

/* Name Manipulation Methods */
const char * elektraKeyName (const ElektraKey * key);
ssize_t elektraKeyNameSize (const ElektraKey * key);

ssize_t elektraKeySetName (ElektraKey * key, const char * newname);
ssize_t elektraKeyAddName (ElektraKey * key, const char * addName);

const char * elektraKeyEscapedName (const ElektraKey * key);
ssize_t elektraKeyEscapedNameSize (const ElektraKey * key);

const char * elektraKeyBaseName (const ElektraKey * key);
ssize_t elektraKeyBaseNameSize (const ElektraKey * key);

ssize_t elektraKeySetBaseName (ElektraKey * key, const char * baseName);
ssize_t elektraKeyAddBaseName (ElektraKey * key, const char * baseName);

elektraNamespace elektraKeyNamespace (ElektraKey const * key);
ssize_t elektraKeySetNamespace (ElektraKey * key, elektraNamespace ns);

/* Value Manipulation Methods */
const void * elektraKeyValue (const ElektraKey * key);
ssize_t elektraKeyValueSize (const ElektraKey * key);

ssize_t elektraKeySetValue (ElektraKey * key, const void * value, size_t valueSize);

/* Locking */
int elektraKeyLock (ElektraKey * key, elektraLockFlags what);
int elektraKeyIsLocked (const ElektraKey * key, elektraLockFlags what);

/* Aliases */
ElektraKey * elektraKeyDup (const ElektraKey * source, elektraCopyFlags flags);

/**************************************
 *
 * KeySet methods
 *
 **************************************/

ElektraKeySet * elektraKeysetNew (size_t alloc, ...);
ElektraKeySet * elektraKeysetVNew (size_t alloc, va_list ap);

ElektraKeySet * elektraKeysetDup (const ElektraKeySet * source);
int elektraKeysetCopy (ElektraKeySet * dest, const ElektraKeySet * source);

uint16_t elektraKeysetIncRef (ElektraKeySet * ks);
uint16_t elektraKeysetDecRef (ElektraKeySet * ks);
uint16_t elektraKeysetGetRef (const ElektraKeySet * ks);

int elektraKeysetClear (ElektraKeySet * ks);
int elektraKeysetDel (ElektraKeySet * ks);

ssize_t elektraKeysetSize (const ElektraKeySet * ks);

ssize_t elektraKeysetAdd (ElektraKeySet * ks, ElektraKey * k);
ElektraKey * elektraKeysetGet (ElektraKeySet * ks, elektraCursor cursor);
ElektraKey * elektraKeysetRemove (ElektraKeySet * ks, elektraCursor cursor);

ssize_t elektraKeysetAddAll (ElektraKeySet * ks, const ElektraKeySet * other);

ElektraKey * elektraKeysetLookup (ElektraKeySet * ks, const ElektraKey * k);
ElektraKey * elektraKeysetLookupByName (ElektraKeySet * ks, const char *name);

ssize_t elektraKeysetSearch (const ElektraKeySet * ks, const ElektraKey * k);

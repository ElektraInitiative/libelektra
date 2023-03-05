#ifndef ELEKTRA_CORE_PUBLIC_H
#define ELEKTRA_CORE_PUBLIC_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// indicates special rules for constness, see comments at use-site for details
#define ELEKTRA_CONST const

typedef uint8_t ElektraNamespace;
enum
{
	ELEKTRA_NS_NONE = 0,
	ELEKTRA_NS_CASCADING = 1,
	ELEKTRA_NS_META = 2,
	ELEKTRA_NS_SPEC = 3,
	ELEKTRA_NS_PROC = 4,
	ELEKTRA_NS_DIR = 5,
	ELEKTRA_NS_USER = 6,
	ELEKTRA_NS_SYSTEM = 7,
	ELEKTRA_NS_DEFAULT = 8,
};

/**
 * @note Elektra's API only returns or accepts this struct as pass-by-value, i.e. not as a pointer.
 *       This is done to emphasize that this struct is stable and its contents and layout will never change,
 *       unlike the other struct types (`ElektraEntry`, `ElektraSet`) used by the API.
 */
typedef struct
{
	const char * name;
	const size_t size;
} ElektraName;

/**
 * @note Elektra's API only returns or accepts this struct as pass-by-value, i.e. not as a pointer.
 *       This is done to emphasize that this struct is stable and its contents and layout will never change,
 *       unlike the other struct types (`ElektraEntry`, `ElektraSet`) used by the API.
 */
typedef struct
{
	const void * value;
	const size_t size;
} ElektraValue;

typedef struct ElektraEntry ElektraEntry;
typedef struct ElektraSet ElektraSet;

// 0 always indicates success, other values depend on function
typedef int ElektraReturnCode;

typedef enum
{
	ELEKTRA_KEY_CP_NAME = 1 << 0,
	ELEKTRA_KEY_CP_VALUE = 1 << 1,
	ELEKTRA_KEY_CP_META = 1 << 2,
	ELEKTRA_KEY_CP_ALL = ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE | ELEKTRA_KEY_CP_META,
} ElektraEntryCopyFlag;

ElektraEntry * elektraEntryNew (ElektraNamespace ns, ElektraName name);
/**
 * @returns @p key, constness follows argument
 */
ELEKTRA_CONST ElektraEntry * elektraEntryIncRefCount (const ElektraEntry * key);
/**
 * @returns @p key, constness follows argument
 */
ELEKTRA_CONST ElektraEntry * elektraEntryDecRefCount (const ElektraEntry * key);
void elektraEntryDel (const ElektraEntry * key);

ElektraNamespace elektraEntryGetNamespace (const ElektraEntry * key);
void elektraEntrySetNamespace (const ElektraEntry * key, ElektraNamespace ns);

ElektraName elektraEntryGetName (const ElektraEntry * key);
// Note: creates copy of name
ElektraReturnCode elektraEntrySetName (ElektraEntry * key, ElektraName name);
// increments nameLock
void elektraEntryLockName (ElektraEntry * key);
// decrements nameLock
void elektraEntryUnlockName (ElektraEntry * key);
bool elektraEntryIsNameLocked (const ElektraEntry * key);

ElektraValue elektraEntryGetValue (const ElektraEntry * key);
// Note: creates copy of value
ElektraReturnCode elektraEntrySetValue (ElektraEntry * key, ElektraValue value);

/**
 * @returns a `const ElektraSet *` that can safely be cast to `ElektraSet *`, iff a non-const `ElektraEntry *` was passed as @p key
 */
ELEKTRA_CONST ElektraSet * elektraEntryGetMeta (const ElektraEntry * key);

ElektraReturnCode elektraEntryIsBelow (const ElektraEntry * first, const ElektraEntry * second);
ElektraReturnCode elektraEntryCompare (const ElektraEntry * first, const ElektraEntry * second);

ElektraEntry * elektraEntryCopy (ElektraEntry * dest, const ElektraEntry * src, ElektraEntryCopyFlag flags);
static inline ElektraEntry * elektraEntryDup (ElektraEntry * key, ElektraEntryCopyFlag flags)
{
	ElektraEntry * dest = flags & ELEKTRA_KEY_CP_NAME ? elektraEntryNew (elektraEntryGetNamespace (key), elektraEntryGetName (key)) :
								  elektraEntryNew (ELEKTRA_NS_CASCADING, (ElektraName){ .name = "", .size = 0 });
	return elektraEntryCopy (dest, key, (ElektraEntryCopyFlag) (flags & ~ELEKTRA_KEY_CP_NAME));
}


ElektraSet * elektraSetNew (size_t prealloc);
/**
 * @returns @p ks, constness follows argument
 */
ELEKTRA_CONST ElektraSet * elektraSetIncRefCount (const ElektraSet * ks);
/**
 * @returns @p ks, constness follows argument
 */
ELEKTRA_CONST ElektraSet * elektraSetDecRefCount (const ElektraSet * ks);
void elektraSetDel (const ElektraSet * ks);

size_t elektraSetSize (ElektraSet * ks);

ElektraReturnCode elektraSetInsert (ElektraSet * ks, ElektraEntry * key);
ElektraReturnCode elektraSetInsertAll (ElektraSet * ks, ElektraSet * other);

ElektraEntry * elektraSetGet (ElektraSet * ks, size_t index);
ElektraSet * elektraSetGetRange (ElektraSet * ks, size_t start, size_t end);

void elektraSetRemove (ElektraSet * ks, size_t index);
void elektraSetRemoveRange (ElektraSet * ks, size_t start, size_t end);
static inline void elektraSetClear (ElektraSet * ks)
{
	elektraSetRemoveRange (ks, 0, elektraSetSize (ks));
}

size_t elektraSetLookup (ElektraSet * ks, ElektraNamespace ns, ElektraName name);
static inline size_t elektraSetLookupEntry (ElektraSet * ks, const ElektraEntry * entry)
{
	return elektraSetLookup (ks, elektraEntryGetNamespace (entry), elektraEntryGetName (entry));
}

size_t elektraSetFindHierarchy (ElektraSet * ks, ElektraNamespace ns, ElektraName root, size_t * end);

#endif // ELEKTRA_CORE_PUBLIC_H

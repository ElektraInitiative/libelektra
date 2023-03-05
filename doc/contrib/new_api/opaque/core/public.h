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

typedef struct ElektraNameCow ElektraName;
typedef struct ElektraEntry ElektraEntry;
typedef struct ElektraSet ElektraSet;

// 0 always indicates success, other values depend on function
typedef int ElektraReturnCode;

typedef enum
{
	ELEKTRA_ENTRY_CP_NAME = 1 << 0,
	ELEKTRA_ENTRY_CP_VALUE = 1 << 1,
	ELEKTRA_ENTRY_CP_META = 1 << 2,
	ELEKTRA_ENTRY_CP_ALL = ELEKTRA_ENTRY_CP_NAME | ELEKTRA_ENTRY_CP_VALUE | ELEKTRA_ENTRY_CP_META,
} ElektraEntryCopyFlag;

ElektraName * elektraNameNew (ElektraNamespace ns, const char * name, size_t size);

ElektraNamespace elektraNameGetNamespace (const ElektraName * name);
const char * elektraNameGetName (const ElektraName * name);
size_t elektraNameGetSize (const ElektraName * name);

/**
 * @returns @p name, constness follows argument
 */
ELEKTRA_CONST ElektraName * elektraNameIncRefCount (const ElektraName * name);
/**
 * @returns @p name, constness follows argument
 */
ELEKTRA_CONST ElektraName * elektraNameDecRefCount (const ElektraName * name);

void elektraNameDel (const ElektraName * name);


ElektraEntry * elektraEntryNew (const ElektraName * name);
/**
 * @returns @p entry, constness follows argument
 */
ELEKTRA_CONST ElektraEntry * elektraEntryIncRefCount (const ElektraEntry * entry);
/**
 * @returns @p entry, constness follows argument
 */
ELEKTRA_CONST ElektraEntry * elektraEntryDecRefCount (const ElektraEntry * entry);
void elektraEntryDel (const ElektraEntry * entry);

const ElektraName * elektraEntryGetName (const ElektraEntry * entry);

/**
 * @note consumes @p name, i.e., calls `elektraNameDel (name)`.
 *       This means the function is safe to call as `elektraSetName (entry, elektraEntryGetName (other))`
 *       or as `elektraSetName (entry, elektraNameNew (ns, name, size))`, but when you use
 *       `elektraSetName (entry, name)` you may need to `elektraNameIncRef (name)` & `elektraNameDecRef (name)`.
 */
ElektraReturnCode elektraEntrySetName (ElektraEntry * entry, const ElektraName * name);
// increments nameLock
void elektraEntryLockName (ElektraEntry * entry);
// decrements nameLock
void elektraEntryUnlockName (ElektraEntry * entry);
bool elektraEntryIsNameLocked (const ElektraEntry * entry);

/**
 * if `sizePtr != NULL` will set `*sizePtr` to size of value in @p entry
 */
const void * elektraEntryGetValue (const ElektraEntry * entry, size_t * sizePtr);

// Note: creates copy of value
ElektraReturnCode elektraEntrySetValue (ElektraEntry * entry, const void * value, size_t size);

/**
 * @returns a `const ElektraSet *` that can safely be cast to `ElektraSet *`, iff a non-const `ElektraEntry *` was passed as @p entry
 */
ELEKTRA_CONST ElektraSet * elektraEntryGetMeta (const ElektraEntry * entry);

ElektraReturnCode elektraEntryIsBelow (const ElektraEntry * first, const ElektraEntry * second);
ElektraReturnCode elektraEntryCompare (const ElektraEntry * first, const ElektraEntry * second);

ElektraEntry * elektraEntryCopy (ElektraEntry * dest, const ElektraEntry * src, ElektraEntryCopyFlag flags);
static inline ElektraEntry * elektraEntryDup (ElektraEntry * entry, ElektraEntryCopyFlag flags)
{
	ElektraEntry * dest = flags & ELEKTRA_ENTRY_CP_NAME ? elektraEntryNew (elektraEntryGetName (entry)) :
								    elektraEntryNew (elektraNameNew (ELEKTRA_NS_CASCADING, "", 0));
	return elektraEntryCopy (dest, entry, (ElektraEntryCopyFlag) (flags & ~ELEKTRA_ENTRY_CP_NAME));
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

ElektraReturnCode elektraSetInsert (ElektraSet * ks, ElektraEntry * entry);
ElektraReturnCode elektraSetInsertAll (ElektraSet * ks, ElektraSet * other);

ElektraEntry * elektraSetGet (ElektraSet * ks, size_t index);
ElektraSet * elektraSetGetRange (ElektraSet * ks, size_t start, size_t end);

void elektraSetRemove (ElektraSet * ks, size_t index);
void elektraSetRemoveRange (ElektraSet * ks, size_t start, size_t end);
static inline void elektraSetClear (ElektraSet * ks)
{
	elektraSetRemoveRange (ks, 0, elektraSetSize (ks));
}

/**
 * @note consumes @p name, i.e., calls `elektraNameDel (name)`.
 *       This means the function is safe to call as `elektraSetLookup (ks, elektraEntryGetName (entry))`
 *       or as `elektraSetLookup (ks, elektraNameNew (ns, name, size))`, but when you use
 *       `elektraSetLookup (ks, name)` you may need to `elektraNameIncRef (name)` & `elektraNameDecRef (name)`.
 */
size_t elektraSetLookup (ElektraSet * ks, const ElektraName * name);

/**
 * @note consumes @p root, i.e., calls `elektraNameDel (root)`.
 *       This means the function is safe to call as `elektraSetFindHierarchy (ks, elektraEntryGetName (entry))`
 *       or as `elektraSetFindHierarchy (ks, elektraNameNew (ns, name, size))`, but when you use
 *       `elektraSetFindHierarchy (ks, root)` you may need to `elektraNameIncRef (root)` & `elektraNameDecRef (root)`.
 */
size_t elektraSetFindHierarchy (ElektraSet * ks, const ElektraName * root, size_t * end);

#endif // ELEKTRA_CORE_PUBLIC_H

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

typedef struct
{
	// constness follows containing struct
	// if the containing struct is a `const ElektraName *` then this field must not be modified
	// if the containing struct is a `ElektraName *` or a non-pointer `ElektraName` it is okay to modify
	ELEKTRA_CONST char * name;
	size_t size;
	ElektraNamespace ns;
} ElektraName;

typedef struct
{
	const void * value;
	size_t size;
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

ElektraEntry * elektraEntryNew (const ElektraName * name);
/**
 * @returns @p key, constness follows argument
 */
ELEKTRA_CONST ElektraEntry * elektraEntryIncRefCount (const ElektraEntry * key);
/**
 * @returns @p key, constness follows argument
 */
ELEKTRA_CONST ElektraEntry * elektraEntryDecRefCount (const ElektraEntry * key);
void elektraEntryDel (const ElektraEntry * key);

const ElektraName * elektraEntryGetName (const ElektraEntry * key);
// Note: creates copy of name
ElektraReturnCode elektraEntrySetName (ElektraEntry * key, const ElektraName * name);
// increments nameLock
void elektraEntryLockName (ElektraEntry * key);
// decrements nameLock
void elektraEntryUnlockName (ElektraEntry * key);
bool elektraEntryIsNameLocked (const ElektraEntry * key);

const ElektraValue * elektraEntryGetValue (const ElektraEntry * key);
// Note: creates copy of value
ElektraReturnCode elektraEntrySetValue (ElektraEntry * key, const ElektraValue * value);

/**
 * @returns a `const ElektraSet *` that can safely be cast to `ElektraSet *`, iff a non-const `ElektraEntry *` was passed as @p key
 */
ELEKTRA_CONST ElektraSet * elektraEntryGetMeta (const ElektraEntry * key);

ElektraReturnCode elektraEntryIsBelow (const ElektraEntry * first, const ElektraEntry * second);
ElektraReturnCode elektraEntryCompare (const ElektraEntry * first, const ElektraEntry * second);

ElektraEntry * elektraEntryCopy (ElektraEntry * dest, const ElektraEntry * src, ElektraEntryCopyFlag flags);
static inline ElektraEntry * elektraEntryDup (ElektraEntry * key, ElektraEntryCopyFlag flags)
{
	ElektraEntry * dest = flags & ELEKTRA_KEY_CP_NAME ?
				      elektraEntryNew (elektraEntryGetName (key)) :
					    elektraEntryNew (&(ElektraName){ .ns = ELEKTRA_NS_CASCADING, .name = "", .size = 0 });
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

size_t elektraSetLookup (ElektraSet * ks, const ElektraName * name);

size_t elektraSetFindHierarchy (ElektraSet * ks, const ElektraName * root, size_t * end);

#endif // ELEKTRA_CORE_PUBLIC_H

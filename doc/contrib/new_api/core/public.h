#ifndef ELEKTRA_CORE_PUBLIC_H
#define ELEKTRA_CORE_PUBLIC_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

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

void ElektraNamePushPart (ElektraName * name, const char * part);
void ElektraNamePopPart (ElektraName * name);

ElektraEntry * ElektraEntryNew (const ElektraName * name);
ElektraEntry * ElektraEntryRetain (const ElektraEntry * key);
void ElektraEntryRelease (const ElektraEntry * key);

const ElektraName * ElektraEntryGetName (const ElektraEntry * key);
// Note: creates copy of name
ElektraReturnCode ElektraSetName (ElektraEntry * key, const ElektraName * name);
// increments nameLock
void ElektraEntryLockName (ElektraEntry * key);
// decrements nameLock
void ElektraEntryUnlockName (ElektraEntry * key);
bool ElektraEntryIsNameLocked (const ElektraEntry * key);

const ElektraValue * ElektraEntryGetValue (const ElektraEntry * key);
// Note: creates copy of value
ElektraReturnCode ElektraSetValue (ElektraEntry * key, const ElektraValue * value);

/**
 * @returns a `const ElektraSet *` that can safely be cast to `ElektraSet *`, iff a non-const `ElektraEntry *` was passed as @p key
 */
ELEKTRA_CONST ElektraSet * ElektraEntryGetMeta (const ElektraEntry * key);

ElektraReturnCode ElektraEntryIsBelow (const ElektraEntry * first, const ElektraEntry * second);
ElektraReturnCode ElektraEntryCompare (const ElektraEntry * first, const ElektraEntry * second);

ElektraEntry * ElektraEntryCopy (ElektraEntry * dest, const ElektraEntry * src, ElektraEntryCopyFlag flags);
static inline ElektraEntry * ElektraEntryDup (ElektraEntry * key, ElektraEntryCopyFlag flags)
{
	ElektraEntry * dest = flags & ELEKTRA_KEY_CP_NAME ?
				      ElektraEntryNew (ElektraEntryGetName (key)) :
					    ElektraEntryNew (&(ElektraName){ .ns = ELEKTRA_NS_CASCADING, .name = "", .size = 0 });
	return ElektraEntryCopy (dest, key, (ElektraEntryCopyFlag) (flags & ~ELEKTRA_KEY_CP_NAME));
}


ElektraSet * ElektraSetNew (size_t prealloc);
ElektraSet * ElektraSetRetain (const ElektraSet * ks);
void ElektraSetRelease (const ElektraSet * ks);

size_t ElektraSetSize (ElektraSet * ks);

ElektraReturnCode ElektraSetInsert (ElektraSet * ks, ElektraEntry * key);
ElektraReturnCode ElektraSetInsertAll (ElektraSet * ks, ElektraSet * other);
static inline ElektraReturnCode ElektraSetInsertAndRelease (ElektraSet * ks, ElektraEntry * key)
{
	ElektraReturnCode error = ElektraSetInsert (ks, key);
	ElektraEntryRelease (key);
	return error;
}

ElektraEntry * ElektraSetGet (ElektraSet * ks, size_t index);
ElektraSet * ElektraSetGetRange (ElektraSet * ks, size_t start, size_t end);

void ElektraSetRemove (ElektraSet * ks, size_t index);
void ElektraSetRemoveRange (ElektraSet * ks, size_t start, size_t end);
static inline void ElektraSetClear (ElektraSet * ks)
{
	ElektraSetRemoveRange (ks, 0, ElektraSetSize (ks));
}

size_t ElektraSetLookup (ElektraSet * ks, const ElektraName * name);

size_t ElektraSetFindHierarchy (ElektraSet * ks, const ElektraName * root, size_t * end);

#endif // ELEKTRA_CORE_PUBLIC_H

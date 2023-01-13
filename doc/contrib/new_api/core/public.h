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
	// if the containing struct is a `const ElektraKeyname *` then this field must not be modified
	// if the containing struct is a `ElektraKeyname *` or a non-pointer `ElektraKeyname` it is okay to modify
	ELEKTRA_CONST char * name;
	size_t size;
	ElektraNamespace ns;
} ElektraKeyname;

typedef struct
{
	const void * value;
	size_t size;
} ElektraKeyvalue;

typedef struct ElektraKey ElektraKey;
typedef struct ElektraKeyset ElektraKeyset;

// 0 always indicates success, other values depend on function
typedef int ElektraReturnCode;

typedef enum
{
	ELEKTRA_KEY_CP_NAME = 1 << 0,
	ELEKTRA_KEY_CP_VALUE = 1 << 1,
	ELEKTRA_KEY_CP_META = 1 << 2,
	ELEKTRA_KEY_CP_ALL = ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE | ELEKTRA_KEY_CP_META,
} ElektraKeyCopyFlag;

void elektraKeynamePushPart (ElektraKeyname * name, const char * part);
void elektraKeynamePopPart (ElektraKeyname * name);

ElektraKey * elektraKeyNew (const ElektraKeyname * name);
ElektraKey * elektraKeyRetain (const ElektraKey * key);
void elektraKeyRelease (const ElektraKey * key);

const ElektraKeyname * elektraKeyGetName (const ElektraKey * key);
// Note: creates copy of name
ElektraReturnCode elektraKeySetName (ElektraKey * key, const ElektraKeyname * name);
// increments nameLock
void elektraKeyLockName (ElektraKey * key);
// decrements nameLock
void elektraKeyUnlockName (ElektraKey * key);
bool elektraKeyIsNameLocked (const ElektraKey * key);

const ElektraKeyvalue * elektraKeyGetValue (const ElektraKey * key);
// Note: creates copy of value
ElektraReturnCode elektraKeySetValue (ElektraKey * key, const ElektraKeyvalue * value);

/**
 * @returns a `const ElektraKeyset *` that can safely be cast to `ElektraKeyset *`, iff a non-const `ElektraKey *` was passed as @p key
 */
ELEKTRA_CONST ElektraKeyset * elektraKeyGetMeta (const ElektraKey * key);

ElektraReturnCode elektraKeyIsBelow (const ElektraKey * first, const ElektraKey * second);
ElektraReturnCode elektraKeyCompare (const ElektraKey * first, const ElektraKey * second);

ElektraKey * elektraKeyCopy (ElektraKey * dest, const ElektraKey * src, ElektraKeyCopyFlag flags);
static inline ElektraKey * elektraKeyDup (ElektraKey * key, ElektraKeyCopyFlag flags)
{
	ElektraKey * dest = flags & ELEKTRA_KEY_CP_NAME ?
				    elektraKeyNew (elektraKeyGetName (key)) :
					  elektraKeyNew (&(ElektraKeyname){ .ns = ELEKTRA_NS_CASCADING, .name = "", .size = 0 });
	return elektraKeyCopy (dest, key, (ElektraKeyCopyFlag) (flags & ~ELEKTRA_KEY_CP_NAME));
}


ElektraKeyset * elektraKeysetNew (size_t prealloc);
ElektraKeyset * elektraKeysetRetain (const ElektraKeyset * ks);
void elektraKeysetRelease (const ElektraKeyset * ks);

size_t elektraKeysetSize (ElektraKeyset * ks);

ElektraReturnCode elektraKeysetInsert (ElektraKeyset * ks, ElektraKey * key);
ElektraReturnCode elektraKeysetInsertAll (ElektraKeyset * ks, ElektraKeyset * other);
static inline ElektraReturnCode elektraKeysetInsertAndRelease (ElektraKeyset * ks, ElektraKey * key)
{
	ElektraReturnCode error = elektraKeysetInsert (ks, key);
	elektraKeyRelease (key);
	return error;
}

ElektraKey * elektraKeysetGet (ElektraKeyset * ks, size_t index);
ElektraKeyset * elektraKeysetGetRange (ElektraKeyset * ks, size_t start, size_t end);

void elektraKeysetRemove (ElektraKeyset * ks, size_t index);
void elektraKeysetRemoveRange (ElektraKeyset * ks, size_t start, size_t end);
static inline void elektraKeysetClear (ElektraKeyset * ks)
{
	elektraKeysetRemoveRange (ks, 0, elektraKeysetSize (ks));
}

size_t elektraKeysetLookup (ElektraKeyset * ks, const ElektraKeyname * name);

size_t elektraKeysetFindHierarchy (ElektraKeyset * ks, const ElektraKeyname * root, size_t * end);

#endif // ELEKTRA_CORE_PUBLIC_H

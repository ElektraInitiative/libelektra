/**
 * @file
 *
 * @brief Proposed declarations.
 *
 * These functions are likely not API/ABI stable.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef KDBPROPOSAL_H
#define KDBPROPOSAL_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif


// can be made simply without elektra's internals, so better keep it as
// extension.
ssize_t keySetStringF (Key * key, const char * format, ...);

int elektraKsToMemArray (KeySet * ks, Key ** buffer);

KeySet * ksRenameKeys (KeySet * config, const Key * name);

/**
 * @brief Lock options
 *
 * @ingroup proposal
 */
enum elektraLockOptions
{
	KEY_LOCK_NAME = 1 << 17,
	KEY_LOCK_VALUE = 1 << 18,
	KEY_LOCK_META = 1 << 19
};

/**
 * @brief More lookup options
 *
 * @ingroup proposal
 */
enum elektraLookupOptions
{
	KDB_O_SPEC = 1 << 15,	///< Use the passed key as specification, instead of looking up the specification first
	KDB_O_CREATE = 1 << 16,      ///< Create the key if it was not found
	KDB_O_NOCASCADING = 1 << 17, ///< Disable cascading search for keys starting with /
	KDB_O_NOSPEC = 1 << 18,      ///< Do not use specification for cascading keys (internal)
	KDB_O_NODEFAULT = 1 << 19,   ///< Do not honor the default spec (internal)
	KDB_O_CALLBACK = 1 << 20     ///< For spec/ lookups that traverse deeper into hierarchy (callback in ksLookup())
};

// locks a key, is this needed externally?
int keyLock (Key * key, option_t what);

// this might become the new keySetName
ssize_t elektraKeySetName (Key * key, const char * newName, option_t options);

KeySet * elektraKeyGetMetaKeySet (const Key * key);

Key * ksPrev (KeySet * ks);
Key * ksPopAtCursor (KeySet * ks, cursor_t c);


typedef enum {
	ELEKTRA_REL_BELOW_SAME_NS = 1 << 0,		// Below Same Namespace, cascading namespace matches only cascading namespace
	ELEKTRA_REL_BELOW_IGNORE_NS = 1 << 1,		// Below Ignore Namespace, namespaces are ignored
	ELEKTRA_REL_BELOW_CASCADING_NS = 1 << 2,	// Below (allow) Cascading Namespace, cascading namespace matches all namespaces
	ELEKTRA_REL_DIRECT_BELOW_SAME_NS = 1 << 3,      // Direct Below Same Namespace
	ELEKTRA_REL_DIRECT_BELOW_IGNORE_NS = 1 << 4,    // Direct Below Ignore Namespace
	ELEKTRA_REL_DIRECT_BELOW_CASCADING_NS = 1 << 5, // Direct Below (allow) Cascading Namespace
	ELEKTRA_REL_SILBLING_SAME_NS = 1 << 6,		// Silbling Same Namespace
	ELEKTRA_REL_SILBLING_IGNORE_NS = 1 << 7,	// Silbling Ignore Namespace
	ELEKTRA_REL_SILBLING_CASCADING_NS = 1 << 8,     // Silbling (allow) Cascading Namespace
	ELEKTRA_REL_NEPHEW_SAME_NS = 1 << 9,		// Nephew Same Namespace
	ELEKTRA_REL_NEPHEW_IGNORE_NS = 1 << 10,		// Nephew Ignore Namespace
	ELEKTRA_REL_NEPHEW_CASCADING_NS = 1 << 11,      // Nephew (allow) Cascading Namespace
} KeyRelType;

int keyRel2 (const Key * k1, const Key * k2, KeyRelType which);
Key * keyAsCascading (const Key * key);
int keyGetLevelsBelow (const Key * k1, const Key * k2);


#ifdef __cplusplus
}
}
#endif


#endif

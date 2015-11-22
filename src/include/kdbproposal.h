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
namespace ckdb {
extern "C" {
#endif


// can be made simply without elektra's internals, so better keep it as
// extension.
ssize_t keySetStringF(Key *key, const char *format, ...);

int elektraKsToMemArray(KeySet *ks, Key **buffer);
KeySet* elektraRenameKeys(KeySet *config, const char* name);

/**
 * @brief Lock options
 *
 * @ingroup proposal
 */
enum elektraLockOptions
{
	KEY_LOCK_NAME=1<<17,
	KEY_LOCK_VALUE=1<<18,
	KEY_LOCK_META=1<<19
};

/**
 * @brief More lookup options
 *
 * @ingroup proposal
 */
enum elektraLookupOptions
{
	KDB_O_SPEC=1<<15,          ///< Use the passed key as specification, instead of looking up the specification first
	KDB_O_CREATE=1<<16,        ///< Create the key if it was not found
	KDB_O_NOCASCADING=1<<17,   ///< Disable cascading search for keys starting with /
	KDB_O_NOSPEC=1<<18,        ///< Do not use specification for cascading keys (internal)
	KDB_O_NODEFAULT=1<<19,     ///< Do not honor the default spec (internal)
	KDB_O_CALLBACK=1<<20       ///< For spec/ lookups that traverse deeper into hierarchy (callback in ksLookup())
};

/**
 * Elektra currently supported Key namespaces.
 *
 * @ingroup proposal
 * @see kdbGet(), keyGetNamespace()
 */
typedef enum
{
	KEY_NS_NONE=0,          ///< no key given as parameter to keyGetNamespace()
	KEY_NS_EMPTY=1,         ///< key name was empty, e.g. invalid key name
	KEY_NS_META=2,          ///< meta key, i.e. any key name not under other categories
	KEY_NS_CASCADING=3,     ///< cascading key, starts with /, abstract name for any of the namespaces below
	KEY_NS_FIRST=4,         ///< For iteration over namespaces (first element, inclusive)
	KEY_NS_SPEC=4,          ///< spec contains the specification of the other namespaces
	KEY_NS_PROC=5,          ///< proc contains process-specific configuration
	KEY_NS_DIR=6,           ///< dir contains configuration from a specific directory
	KEY_NS_USER=7,          ///< user key in the home directory of the current user
	KEY_NS_SYSTEM=8,        ///< system key is shared for a computer system
	KEY_NS_LAST=8           ///< For iteration over namespaces (last element, inclusive)
} elektraNamespace;

elektraNamespace keyGetNamespace(Key const* key);



// locks a key, is this needed externally?
int keyLock(Key *key,
	/*option_t*/ enum elektraLockOptions lock);

// this might become the new keySetName
ssize_t elektraKeySetName(Key *key, const char *newName,
	option_t options);

KeySet *elektraKeyGetMetaKeySet(const Key *key);

Key *ksPrev(KeySet *ks);
Key *ksPopAtCursor(KeySet *ks, cursor_t c);

#ifdef __cplusplus
}
}
#endif


#endif

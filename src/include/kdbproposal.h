/**
 * @file
 *
 * @brief Proposed declarations.
 *
 * These functions are likely not API/ABI stable.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDBPROPOSAL_H
#define KDBPROPOSAL_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

KeySet * ksRenameKeys (KeySet * config, const Key * name);


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
	KDB_O_CALLBACK = 1 << 20,    ///< For spec/ lookups that traverse deeper into hierarchy (callback in ksLookup())
	KDB_O_OPMPHM = 1 << 21,   ///< Overrule ksLookup search predictor to use OPMPHM, make sure to set ENABLE_OPTIMIZATIONS=ON at cmake
	KDB_O_BINSEARCH = 1 << 22 ///< Overrule ksLookup search predictor to use Binary search for lookup
};

// this might become the new keySetName
ssize_t elektraKeySetName (Key * key, const char * newName, option_t options);

KeySet * elektraKeyGetMetaKeySet (const Key * key);

Key * ksPrev (KeySet * ks);
Key * ksPopAtCursor (KeySet * ks, cursor_t c);

#ifdef __cplusplus
}
}
#endif


#endif

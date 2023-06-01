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

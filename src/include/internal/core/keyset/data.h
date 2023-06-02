/**
 * @file
 *
 * @brief Internal API for keyset data
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CORE_KEYSET_DATA_INTERNAL_H
#define ELEKTRA_CORE_KEYSET_DATA_INTERNAL_H

#include <elektra/config.h>

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
#include <internal/core/opmphm.h>
#include <internal/core/opmphmpredictor.h>
#endif

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

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

static inline bool isKeySetDataInMmap (const struct _KeySetData * keysetdata)
{
	return keysetdata->isInMmap;
}

static inline void setKeySetDataIsInMmap (struct _KeySetData * keysetdata, bool isInMmap)
{
	keysetdata->isInMmap = isInMmap;
}

#endif // ELEKTRA_CORE_KEYSET_DATA_INTERNAL_H

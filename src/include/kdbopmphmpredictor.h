/**
 * @file
 *
 * @brief Defines for the Order Preserving Minimal Perfect Hash Map Predictor.
 *
 * @copyright BSD License (see doc/COPYING or https://www.libelektra.org)
 */
#ifndef OPMPHM_PREDICTOR_H
#define OPMPHM_PREDICTOR_H

#include <stdint.h>
#include <stdlib.h>

/**
 * Order Preserving Minimal Perfect Hash Map Predictor
 *
 * A modified Global History Register and Global Pattern History Table
 * branch prediction algorithm from
 *
 * Tse-Yu Yeh and Yale N. Patt
 * "Alternative Implementations of Two-Level Adaptive Branch Prediction"
 * In: The 19th Annual International Symposium on Computer Architecture
 * (1992), pp. 124-134
 *
 * Maps the event of branch taken or not to a sequence of ksLookup (...) invocations
 * without KeySet alteration that is worth using the OPMPHM or not.
 */

/**
 * OPMPHM_PREDICTOR_HISTORY_EXTRACTION_MASK defines the length and extraction mask of the global history register
 * interpreted binary it must be a series of 1, with a minimum value of 0x3 and a maximum value of 0x7FFF.
 */
#define OPMPHM_PREDICTOR_HISTORY_EXTRACTION_MASK 0x7FF // 11 bit history

typedef struct
{
	uint16_t history;       /*!< the global history register */
	uint8_t * patternTable; /*!< the global pattern history table */
	size_t size;		/*!< size of patternTable in bytes */
	size_t lookupCount;     /*!< number of lookups made without alteration of the KeySet */
} OpmphmPredictor;

/**
 * Basic functions
 */
OpmphmPredictor * opmphmPredictorNew (void);
void opmphmPredictorDel (OpmphmPredictor * op);
void opmphmPredictorCopy (OpmphmPredictor * dest, OpmphmPredictor * source);

/**
 * Heuristic function
 */
size_t opmphmPredictorWorthOpmphm (size_t n);

/**
 * Predictor functions
 */
int opmphmPredictor (OpmphmPredictor * op, size_t n);
void opmphmPredictorIncCountOpmphm (OpmphmPredictor * op);
int opmphmPredictorIncCountBinarySearch (OpmphmPredictor * op, size_t n);

#endif

/**
 * @file
 *
 * @brief The Order Preserving Minimal Perfect Hash Map Predictor.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbassert.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbopmphmpredictor.h>
#include <string.h>

/**
 * Prediction Automata A2
 *
 * state 0 and 1 are not worth using the OPMPHM
 * state 2 and 3 are worth using the OPMPHM
 */
static uint8_t predictionAutomata[4][2] = {
	// state/0v1	 0 1
	// clang-format off
	/* 0 */		{0,1},
	/* 1 */		{0,2},
	/* 2 */		{1,3},
	/* 3 */		{2,3}
	// clang-format on
};

/**
 * @brief Heuristic function above the OPMPHM usage is worth.
 *
 * Tells how many ksLookup (...) invocations without alteration of the KeySet need to be made to justify the OPMPHM usage.
 *
 * @param n the number of elements in the KeySet
 *
 * @retval size_t the heuristic value
 */
inline size_t opmphmPredictorWorthOpmphm (size_t n)
{
	return n + 5000;
}

/**
 * @brief Increases the counter when the OPMPHM was used for the ksLookup (...) .
 *
 * @param op the Predictor
 */
inline void opmphmPredictorIncCountOpmphm (OpmphmPredictor * op)
{
	ELEKTRA_NOT_NULL (op);
	++op->lookupCount;
}

/**
 * @brief Increases the counter when the Binary Search was used for the ksLookup (...) .
 *
 * Prevents also a endless Binary Search usage by a simple heuristic.
 *
 * @param op the Predictor
 * @param n the number of elements in the KeySet
 *
 * @retval 1 it is worth using the OPMPHM
 * @retval 0 it is not worth using the OPMPHM
 */
inline int opmphmPredictorIncCountBinarySearch (OpmphmPredictor * op, size_t n)
{
	ELEKTRA_NOT_NULL (op);
	++op->lookupCount;
	size_t worthOpmphm = opmphmPredictorWorthOpmphm (n);
	worthOpmphm = worthOpmphm + worthOpmphm + worthOpmphm; // 3 * worthOpmphm
	if (op->lookupCount > worthOpmphm)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/**
 * @brief Predictcs at the first ksLookup (...) after a KeySet changed if it will be worth using the OPMPHM.
 *
 * Uses the opmphmPredictorWorthOpmphm (...) to check if the previous sequence of ksLookup (...) invocations without
 * alteration was worth using the OPMPHM. Updates the state with the predictionAutomata and the worth information
 * of the previous history. Alters the history with the worth information and makes the prediction for the
 * next sequence of ksLookup (...) invocations.
 *
 * @param op the Predictor
 * @param n the number of elements in the KeySet
 *
 * @retval 1 it is worth using the OPMPHM
 * @retval 0 it is not worth using the OPMPHM
 */
int opmphmPredictor (OpmphmPredictor * op, size_t n)
{
	ELEKTRA_NOT_NULL (op);
	ELEKTRA_ASSERT (n > 0, "n is 0");
	/*
	 * update patterTable with worth information
	 */
	// check if previos lookup sequence was worth hashing
	size_t worthOpmphm = opmphmPredictorWorthOpmphm (n);
	uint8_t wasItWorth = op->lookupCount > worthOpmphm ? 1 : 0;
	// find position in array
	uint16_t pos = op->history & OPMPHM_PREDICTOR_HISTORY_EXTRACTION_MASK;
	uint8_t * state = &(op->patternTable[pos >> 2]); // (pos * 2) / 8 == pos >> 2
	// extract 2 bit state
	uint8_t newState = (*state >> ((pos & 0x3) << 1)) & 0x3; // ((pos & 0x3) << 1) == (pos % 4) * 2), 0x3 is 2 bit mask
	// feed state and worth information to the predictionAutomata
	newState = predictionAutomata[newState][wasItWorth];
	// delete old state
	*state &= ~(0x3 << ((pos & 0x3) << 1)); // ((pos & 0x3) << 1) == (pos % 4) * 2), 0x3 is 2 bit mask
	// store the new state
	*state |= newState << ((pos & 0x3) << 1); // ((pos & 0x3) << 1) == (pos % 4) * 2), 0x3 is 2 bit mask
	/*
	 * predict with updated history
	 */
	// add the worth information to the history
	op->history = ((op->history << 1) | wasItWorth) & OPMPHM_PREDICTOR_HISTORY_EXTRACTION_MASK;
	pos = op->history & OPMPHM_PREDICTOR_HISTORY_EXTRACTION_MASK;
	state = &(op->patternTable[pos >> 2]); // (pos * 2) / 8 == pos >> 2
	// extract 2 bit state
	uint8_t prediction = (*state >> ((pos & 0x3) << 1)) & 0x3; // ((pos & 0x3) << 1) == (pos % 4) * 2), 0x3 is 2 bit mask
	// reset lookupCount
	op->lookupCount = 1;
	// determine prediction
	if (prediction > 1)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/**
 * @brief Allocates and initializes the OpmphmPredictor.
 *
 * Reserves for all possible values of OPMPHM_PREDICTOR_HISTORY_EXTRACTION_MASK two bits
 * to store all 4 states of the Prediction Automata A2.
 * Sets the initial state to 0.
 *
 * @retval OpmphmPredictor * success
 * @retval NULL memory error
 */
OpmphmPredictor * opmphmPredictorNew (void)
{
	OpmphmPredictor * out = elektraCalloc (sizeof (OpmphmPredictor));
	if (!out)
	{
		return NULL;
	}
	uint16_t bytesInPatternTable = (OPMPHM_PREDICTOR_HISTORY_EXTRACTION_MASK + 1) >> 2; // x >> 2 == (x * 2) / 8
	if (!bytesInPatternTable)
	{
		bytesInPatternTable = 1;
	}
	out->patternTable = elektraCalloc (bytesInPatternTable * sizeof (uint8_t));
	//~ memset (out->patternTable, 0x0, bytesInPatternTable * sizeof (uint8_t));
	if (!out->patternTable)
	{
		elektraFree (out);
		return NULL;
	}
	out->size = bytesInPatternTable;
	return out;
}

/**
 * @brief Make a copy of the OpmphmPredictor.
 *
 * @param source the OpmphmPredictor source
 * @param dest the OpmphmPredictor destination
 */
void opmphmPredictorCopy (OpmphmPredictor * dest, OpmphmPredictor * source)
{
	ELEKTRA_NOT_NULL (source);
	ELEKTRA_NOT_NULL (dest);
	// copy values
	dest->history = source->history;
	dest->size = source->size;
	dest->lookupCount = source->lookupCount;
	// copy patternTable
	memcpy (dest->patternTable, source->patternTable, source->size * sizeof (uint8_t));
}

/**
 * @brief Deletes the OpmphmPredictor.
 *
 * Clears and frees all memory in OpmphmPredictor.
 *
 * @param op the OpmphmPredictor
 */
void opmphmPredictorDel (OpmphmPredictor * op)
{
	ELEKTRA_NOT_NULL (op);
	elektraFree (op->patternTable);
	elektraFree (op);
}

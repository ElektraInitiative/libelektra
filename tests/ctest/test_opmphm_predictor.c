/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 */

#include <opmphmpredictor.c>
#include <tests_internal.h>

void test_internal_basic (void)
{
	OpmphmPredictor * op = opmphmPredictorNew ();
	exit_if_fail (op, "opmphmPredictorNew");
	succeed_if (!op->history, "history init");
	succeed_if (!op->lookupCount, "lookupCount init");
	succeed_if (op->size, "size init");
	for (size_t i = 0; i < op->size; ++i)
	{
		succeed_if (!op->patternTable[i], "patternTable init");
	}
	opmphmPredictorDel (op);
}

void test_internal_nochange (void)
{
	OpmphmPredictor * op = opmphmPredictorNew ();
	exit_if_fail (op, "opmphmPredictorNew");
	const size_t n = 1000;
	opmphmPredictorIncCountOpmphm (op);
	succeed_if (op->lookupCount == 1, "lookupCount");
	for (size_t i = 1; i < opmphmPredictorWorthOpmphm (n) * 3; ++i)
	{
		// no opmphm
		succeed_if (!opmphmPredictorIncCountBinarySearch (op, n), "binary search usage");
		succeed_if (op->lookupCount == i + 1, "lookupCount");
	}
	// switch to opmphm
	succeed_if (opmphmPredictorIncCountBinarySearch (op, n), "hash usage");
	succeed_if (op->lookupCount == opmphmPredictorWorthOpmphm (n) * 3 + 1, "lookupCount");
	opmphmPredictorDel (op);
}

/**
 * @brief Switches the internal states of the OpmphmPredictor and checks switch.
 *
 * @param oldState old State
 * @param newState new State
 * @param op the OpmphmPredictor
 * @param n number of elements
 */
static void test_internal_change_whitebox_set_to_state (uint8_t oldState, uint8_t newState, OpmphmPredictor * op, size_t n)
{
	size_t setLookupCount;
	if (oldState == 0 && newState == 0)
	{
		// not worth hashing
		setLookupCount = opmphmPredictorWorthOpmphm (n);
	}
	else if (oldState == 3 && newState == 3)
	{
		// worth hashing
		setLookupCount = opmphmPredictorWorthOpmphm (n) + 1;
	}
	else if (oldState + 1 == newState)
	{
		// worth hashing
		setLookupCount = opmphmPredictorWorthOpmphm (n) + 1;
	}
	else if (oldState == newState + 1)
	{
		// not worth hashing
		setLookupCount = opmphmPredictorWorthOpmphm (n);
	}
	else
	{
		exit_if_fail (0, "wrong usage");
	}
	/*
	 * Set every entry in pattern table to state
	 */
	for (uint16_t testHistory = 0; testHistory <= opmphmPredictorHistoryMask; ++testHistory)
	{
		op->history = testHistory;
		// set not worth to hash
		op->lookupCount = setLookupCount;
		// between state 1 and 2 is the prediction reulst transition, and patterns interfere, thus no check
		if (!((oldState == 1 && newState == 2) || (oldState == 2 && newState == 1)))
		{
			if (newState < 2)
			{
				succeed_if (!opmphmPredictor (op, n), "binary search usage");
			}
			else
			{
				succeed_if (opmphmPredictor (op, n), "hash usage");
			}
		}
		else
		{
			// no ckeck
			opmphmPredictor (op, n);
		}
		succeed_if (op->lookupCount == 1, "lookupCount");
	}
	// test pattern
	for (size_t i = 0; i < op->size; ++i)
	{
		if (newState == 0)
		{
			succeed_if (op->patternTable[i] == 0x0, "patternTable state 0"); // 0x0 is 00000000 all 4 states per byte to 0
		}
		else if (newState == 1)
		{
			succeed_if (op->patternTable[i] == 0x55, "patternTable state 1"); // 0x55 is 01010101 all 4 states  per byte to 1
		}
		else if (newState == 2)
		{
			succeed_if (op->patternTable[i] == 0xAA, "patternTable state 2"); // 0xAA is 10101010 all 4 states per byte to 2
		}
		else if (newState == 3)
		{
			succeed_if (op->patternTable[i] == 0xFF, "patternTable state 3"); // 0xFF is 11111111 all 4 states per byte to 3
		}
	}
}


void test_internal_change_whitebox (void)
{
	OpmphmPredictor * op = opmphmPredictorNew ();
	exit_if_fail (op, "opmphmPredictorNew");
	const size_t n = 1000;
	// start state is 0
	test_internal_change_whitebox_set_to_state (0, 0, op, n);
	test_internal_change_whitebox_set_to_state (0, 1, op, n);
	test_internal_change_whitebox_set_to_state (1, 2, op, n);
	test_internal_change_whitebox_set_to_state (2, 3, op, n);
	test_internal_change_whitebox_set_to_state (3, 3, op, n);
	test_internal_change_whitebox_set_to_state (3, 2, op, n);
	test_internal_change_whitebox_set_to_state (2, 1, op, n);
	test_internal_change_whitebox_set_to_state (1, 0, op, n);
	test_internal_change_whitebox_set_to_state (0, 0, op, n);

	opmphmPredictorDel (op);
}


int main (int argc, char ** argv)
{
	printf ("OPMPHM PREDICTOR      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_internal_basic ();
	test_internal_nochange ();
	test_internal_change_whitebox ();

	print_result ("test_opmphm_predictor");

	return nbError;
}

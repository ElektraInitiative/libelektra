/**
 * @file
 *
 * @brief Tests for I/O bindings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <elektra/kdbhelper.h>
#include <tests.h>

#include "test.h"
#include <elektra/kdbio.h>
#include <kdbiotest.h>

#define TIMER_TEST_INTERVAL 250
#define TIMER_TEST_TIMES 3
#define TIMER_DIFF_WARNING_THRESHOLD 5
#define TIMER_DIFF_ERROR_THRESHOLD (TIMER_DIFF_WARNING_THRESHOLD * 100)

// Control interval is 50ms * 5 = 250ms for the probe interval
// To check for the change (enabled or interval) we need control to run 5 + 1 times
#define TIMER_CHANGE_CONTROL_INTERVAL 50
#define TIMER_CHANGE_TIMES 6

#define TIMER_CHANGE_SECOND_INTERVAL TIMER_TEST_INTERVAL - TIMER_CHANGE_CONTROL_INTERVAL
#define TIMER_CHANGE_PROBE_TIMES 2

static ElektraIoTestSuiteStop testStop;
static struct timespec testTimeStarted;

static int testCallbackOnceCalled;
static struct timespec testCallbackOnceTimeCalled;

static int testCallbackAtIntervalsCounter;
static struct timespec testCallbackAtIntervalsTimeCalled[TIMER_TEST_TIMES];

static int testUpdateEnabledControlCalled;
static int testUpdateEnabledProbeCalled;
static ElektraIoTimerOperation * testUpdateEnabledTimerProbe;
static ElektraIoInterface * testUpdateEnabledBinding;

static int testUpdateIntervalControlCalled;
static int testUpdateIntervalProbeCalled;
static ElektraIoTimerOperation * testUpdateIntervalTimerProbe;
static ElektraIoInterface * testUpdateIntervalBinding;
static struct timespec testUpdateIntervalTimeCalled, testUpdateIntervalTimeCalledLast;

static int testRemoveControlCalled;
static int testRemoveProbeCalled;
static ElektraIoTimerOperation * testRemoveTimerProbe;
static ElektraIoInterface * testRemoveBinding;

static void testTimerBasicsCallback (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	yield_error ("should not be called");
}

static void testTimerBasics (ElektraIoTestSuiteCreateBinding createBinding)
{
	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TIMER_TEST_INTERVAL, 0, testTimerBasicsCallback, NULL);

	ElektraIoInterface * binding = createBinding ();
	succeed_if (elektraIoBindingAddTimer (binding, timerOp), "addTimer did not succeed");
	succeed_if (elektraIoBindingAddTimer (binding, timerOp) == 0, "addTimer: should not be able to reassign operation to a binding");

	elektraIoTimerSetEnabled (timerOp, 1);
	succeed_if (elektraIoBindingUpdateTimer (timerOp), "updateTimer did not succeed");

	succeed_if (elektraIoBindingRemoveTimer (timerOp), "removeTimer did not succeed");

	succeed_if (elektraIoBindingAddTimer (binding, timerOp), "addTimer: should be able to assign operation after removal");
	succeed_if (elektraIoBindingRemoveTimer (timerOp), "removeTimer did not succeed");
	elektraIoBindingCleanup (binding);
	elektraFree (timerOp);
}

static void testTimerShouldCallbackOnceElapsed (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	testCallbackOnceCalled = 1;
	elektraIoTestSuiteUtilGetCurrentTime (&testCallbackOnceTimeCalled);
	testStop ();
}

static void testTimerShouldCallbackOnce (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					 ElektraIoTestSuiteStop stop)
{
	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TIMER_TEST_INTERVAL, 1, testTimerShouldCallbackOnceElapsed, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddTimer (binding, timerOp);

	testStop = stop;
	testCallbackOnceCalled = 0;

	elektraIoTestSuiteUtilGetCurrentTime (&testTimeStarted);

	start ();

	succeed_if (testCallbackOnceCalled, "callback was not called");

	long diff = elektraIoTestSuiteUtilGetTimeDifference (testTimeStarted, testCallbackOnceTimeCalled);
	int deviation = labs (TIMER_TEST_INTERVAL - diff);
	if (deviation > TIMER_DIFF_WARNING_THRESHOLD)
	{
		printf ("testTimerShouldCallbackOnce (warning): measured %ldms, expected %dms - deviation %dms.\n", diff,
			TIMER_TEST_INTERVAL, deviation);
	}
	succeed_if (deviation <= TIMER_DIFF_ERROR_THRESHOLD, "timer interval not within error threshold");

	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingCleanup (binding);
	elektraFree (timerOp);
}

static void testTimerShouldCallbackAtIntervalsCallback (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	testCallbackAtIntervalsCounter--;
	elektraIoTestSuiteUtilGetCurrentTime (&testCallbackAtIntervalsTimeCalled[testCallbackAtIntervalsCounter]);

	if (testCallbackAtIntervalsCounter == 0)
	{
		testStop ();
	}
}

static void testTimerShouldCallbackAtIntervals (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
						ElektraIoTestSuiteStop stop)
{
	ElektraIoTimerOperation * timerOp =
		elektraIoNewTimerOperation (TIMER_TEST_INTERVAL, 1, testTimerShouldCallbackAtIntervalsCallback, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddTimer (binding, timerOp);

	testCallbackAtIntervalsCounter = TIMER_TEST_TIMES;
	testStop = stop;

	elektraIoTestSuiteUtilGetCurrentTime (&testTimeStarted);

	start ();

	succeed_if (testCallbackAtIntervalsCounter == 0, "intervals timer not called the required amount of times");

	// Verify intervals
	struct timespec lastTime = testTimeStarted;
	for (int i = TIMER_TEST_TIMES - 1; i >= 0; i--)
	{
		long diff = elektraIoTestSuiteUtilGetTimeDifference (lastTime, testCallbackAtIntervalsTimeCalled[i]);
		int deviation = labs (TIMER_TEST_INTERVAL - diff);
		if (deviation > TIMER_DIFF_WARNING_THRESHOLD)
		{
			printf ("testTimerShouldCallbackAtIntervals (warning): measured %ldms, expected %dms - deviation %dms.\n", diff,
				TIMER_TEST_INTERVAL, deviation);
		}
		succeed_if (deviation <= TIMER_DIFF_ERROR_THRESHOLD, "timer interval not within error threshold");

		lastTime = testCallbackAtIntervalsTimeCalled[i];
	}

	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingCleanup (binding);
	elektraFree (timerOp);
}

static void testTimerShouldChangeEnabledControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	testUpdateEnabledControlCalled--;

	// Disable probe timer on first run
	if (testUpdateEnabledControlCalled == TIMER_CHANGE_TIMES - 1)
	{
		elektraIoTimerSetEnabled (testUpdateEnabledTimerProbe, 0);
		elektraIoBindingUpdateTimer (testUpdateEnabledTimerProbe);
	}

	if (testUpdateEnabledControlCalled == 0 || testUpdateEnabledProbeCalled > 1)
	{
		testStop ();
	}
}

static void testTimerShouldChangeEnabledProbe (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	testUpdateEnabledProbeCalled++;
}

static void testTimerShouldChangeEnabled (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					  ElektraIoTestSuiteStop stop)
{
	ElektraIoTimerOperation * timerControl =
		elektraIoNewTimerOperation (TIMER_CHANGE_CONTROL_INTERVAL, 1, testTimerShouldChangeEnabledControl, NULL);

	ElektraIoTimerOperation * timerProbe = elektraIoNewTimerOperation (TIMER_TEST_INTERVAL, 1, testTimerShouldChangeEnabledProbe, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddTimer (binding, timerControl);
	elektraIoBindingAddTimer (binding, timerProbe);

	testStop = stop;
	testUpdateEnabledControlCalled = TIMER_CHANGE_TIMES;
	testUpdateEnabledProbeCalled = 0;
	testUpdateEnabledTimerProbe = timerProbe;
	testUpdateEnabledBinding = binding;

	start ();

	succeed_if (testUpdateEnabledProbeCalled == 0, "timer callback was not disabled");
	succeed_if (testUpdateEnabledControlCalled == 0, "timout control callback was not called required amount of times");

	elektraIoBindingRemoveTimer (timerControl);
	elektraIoBindingRemoveTimer (timerProbe);
	elektraIoBindingCleanup (binding);
	elektraFree (timerControl);
	elektraFree (timerProbe);
}

static void testTimerShouldChangeIntervalControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	testUpdateIntervalControlCalled++;

	// Change probe interval on first run, before probe was run
	if (testUpdateIntervalControlCalled == 1)
	{
		elektraIoTimerSetInterval (testUpdateIntervalTimerProbe, TIMER_CHANGE_SECOND_INTERVAL);
		elektraIoBindingUpdateTimer (testUpdateIntervalTimerProbe);
	}

	if (testUpdateIntervalProbeCalled > TIMER_CHANGE_PROBE_TIMES)
	{
		testStop ();
	}
}

static void testTimerShouldChangeIntervalProbe (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	testUpdateIntervalTimeCalledLast = testUpdateIntervalTimeCalled;
	elektraIoTestSuiteUtilGetCurrentTime (&testUpdateIntervalTimeCalled);

	testUpdateIntervalProbeCalled++;
}

static void testTimerShouldChangeInterval (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					   ElektraIoTestSuiteStop stop)
{
	// Control timer will change interval
	ElektraIoTimerOperation * timerControl =
		elektraIoNewTimerOperation (TIMER_CHANGE_CONTROL_INTERVAL, 1, testTimerShouldChangeIntervalControl, NULL);

	// Probe will just count and measure time
	ElektraIoTimerOperation * timerProbe =
		elektraIoNewTimerOperation (TIMER_TEST_INTERVAL, 1, testTimerShouldChangeIntervalProbe, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddTimer (binding, timerControl);
	elektraIoBindingAddTimer (binding, timerProbe);

	testStop = stop;
	testUpdateIntervalControlCalled = 0;
	testUpdateIntervalProbeCalled = 0;
	testUpdateIntervalTimerProbe = timerProbe;
	testUpdateIntervalBinding = binding;

	elektraIoTestSuiteUtilGetCurrentTime (&testTimeStarted);

	start ();

	succeed_if (testUpdateIntervalProbeCalled == TIMER_TEST_TIMES, "timer was not called the required amount of times");

	// Verify last interval
	long diff = elektraIoTestSuiteUtilGetTimeDifference (testUpdateIntervalTimeCalledLast, testUpdateIntervalTimeCalled);
	int deviation = labs (TIMER_CHANGE_SECOND_INTERVAL - diff);
	if (deviation > TIMER_DIFF_WARNING_THRESHOLD)
	{
		printf ("testTimerShouldCallbackAtIntervals (warning): measured %ldms, expected %dms - deviation %dms.\n", diff,
			TIMER_CHANGE_SECOND_INTERVAL, deviation);
	}
	succeed_if (deviation <= TIMER_DIFF_ERROR_THRESHOLD, "timer interval not within threshold");

	elektraIoBindingRemoveTimer (timerControl);
	elektraIoBindingRemoveTimer (timerProbe);
	elektraIoBindingCleanup (binding);
	elektraFree (timerControl);
	elektraFree (timerProbe);
}

static void testTimerShouldRemoveControl (ElektraIoTimerOperation * idleInfo ELEKTRA_UNUSED)
{
	testRemoveControlCalled--;

	// Disable probe timer on first run
	if (testRemoveControlCalled == TIMER_CHANGE_TIMES - 1)
	{
		elektraIoBindingRemoveTimer (testRemoveTimerProbe);
	}

	if (testRemoveControlCalled == 0 || testRemoveProbeCalled > 1)
	{
		testStop ();
	}
}

static void testTimerShouldRemoveProbe (ElektraIoTimerOperation * idleInfo ELEKTRA_UNUSED)
{
	testRemoveProbeCalled++;
}

static void testTimerShouldRemove (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
				   ElektraIoTestSuiteStop stop)
{
	ElektraIoTimerOperation * timerControl =
		elektraIoNewTimerOperation (TIMER_CHANGE_CONTROL_INTERVAL, 1, testTimerShouldRemoveControl, NULL);

	ElektraIoTimerOperation * timerProbe = elektraIoNewTimerOperation (TIMER_TEST_INTERVAL, 1, testTimerShouldRemoveProbe, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddTimer (binding, timerControl);
	elektraIoBindingAddTimer (binding, timerProbe);

	testStop = stop;
	testRemoveControlCalled = TIMER_CHANGE_TIMES;
	testRemoveProbeCalled = 0;
	testRemoveTimerProbe = timerProbe;
	testRemoveBinding = binding;

	start ();

	succeed_if (testRemoveProbeCalled == 0, "timer callback was not removed");
	succeed_if (testRemoveControlCalled == 0, "timout control callback was not called required amount of times");

	elektraIoBindingRemoveTimer (timerControl);
	if (testRemoveProbeCalled != 0)
	{
		elektraIoBindingRemoveTimer (timerProbe);
	}
	elektraIoBindingCleanup (binding);
	elektraFree (timerControl);
	elektraFree (timerProbe);
}

/**
 * Test timer functions of the I/O binding returned by createBinding.
 * Requires the following operations: Timer
 *
 * @param createBinding binding creation function
 * @param start         starts I/O operations
 * @param stop          stops I/O operations
 */
void elektraIoTestSuiteTimer (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	printf ("test timer\n");

	testTimerBasics (createBinding);

	testTimerShouldCallbackOnce (createBinding, start, stop);

	testTimerShouldCallbackAtIntervals (createBinding, start, stop);

	testTimerShouldChangeEnabled (createBinding, start, stop);

	testTimerShouldChangeInterval (createBinding, start, stop);

	testTimerShouldRemove (createBinding, start, stop);
}

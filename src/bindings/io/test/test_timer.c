/**
 * @file
 *
 * @brief Tests for haskelltemplate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdbhelper.h>
#include <tests.h>

#include "test.h"
#include <kdbio.h>

#define TIMER_TEST_INTERVAL 50
#define TIMER_TEST_TIMES 3
#define TIMER_DIFF_WARNING_THRESHOLD 5
#define TIMER_DIFF_ERROR_THRESHOLD (TIMER_DIFF_WARNING_THRESHOLD * 100)

// Control interval is 10ms * 5 = 50ms for the probe interval
// To check for the change (enabled or interval) we need control to run 5 + 1 times
#define TIMER_CHANGE_CONTROL_INTERVAL 10
#define TIMER_CHANGE_TIMES 6

#define TIMER_CHANGE_SECOND_INTERVAL TIMER_TEST_INTERVAL - TIMER_CHANGE_CONTROL_INTERVAL
#define TIMER_CHANGE_PROBE_TIMES 2

ElektraIoTestSuiteStop testStop;
struct timespec testTimeStarted;

int testCallbackOnceCalled;
struct timespec testCallbackOnceTimeCalled;

int testCallbackAtIntervalsCounter;
struct timespec testCallbackAtIntervalsTimeCalled[TIMER_TEST_TIMES];

int testUpdateEnabledControlCalled;
int testUpdateEnabledProbeCalled;
ElektraIoTimerOperation * testUpdateEnabledTimerProbe;
ElektraIoInterface * testUpdateEnabledBinding;

int testUpdateIntervalControlCalled;
int testUpdateIntervalProbeCalled;
ElektraIoTimerOperation * testUpdateIntervalTimerProbe;
ElektraIoInterface * testUpdateIntervalBinding;
struct timespec testUpdateIntervalTimeCalled, testUpdateIntervalTimeCalledLast;

int testRemoveControlCalled;
int testRemoveProbeCalled;
ElektraIoTimerOperation * testRemoveTimerProbe;
ElektraIoInterface * testRemoveBinding;

static void testTimerBasicsCallback (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	yield_error ("should not be called");
}

static void testTimerBasics (ElektraIoTestSuiteCreateBinding createBinding)
{
	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = 100;
	timerOp->enabled = 0;
	timerOp->callback = testTimerBasicsCallback;

	ElektraIoInterface * binding = createBinding ();
	succeed_if (binding->addTimer (binding, timerOp) == 0, "addTimer did not return 0");
	succeed_if (timerOp->binding == binding, "binding was not set");

	timerOp->enabled = 1;
	succeed_if (binding->updateTimer (timerOp) == 0, "updateTimer did not return 0");

	succeed_if (binding->removeTimer (timerOp) == 0, "removeTimer did not return 0");
	binding->cleanup (binding);
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
	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = TIMER_TEST_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testTimerShouldCallbackOnceElapsed;

	ElektraIoInterface * binding = createBinding ();
	binding->addTimer (binding, timerOp);

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

	binding->removeTimer (timerOp);
	binding->cleanup (binding);
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
	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = TIMER_TEST_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testTimerShouldCallbackAtIntervalsCallback;

	ElektraIoInterface * binding = createBinding ();
	binding->addTimer (binding, timerOp);

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

	binding->removeTimer (timerOp);
	binding->cleanup (binding);
	elektraFree (timerOp);
}

static void testTimerShouldChangeEnabledControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	testUpdateEnabledControlCalled--;

	// Disable probe timer on first run
	if (testUpdateEnabledControlCalled == TIMER_CHANGE_TIMES - 1)
	{
		testUpdateEnabledTimerProbe->enabled = 0;
		testUpdateEnabledBinding->updateTimer (testUpdateEnabledTimerProbe);
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
	ElektraIoTimerOperation * timerControl = elektraIoTestSuiteUtilNewTimerOperation ();
	timerControl->interval = TIMER_CHANGE_CONTROL_INTERVAL;
	timerControl->enabled = 1;
	timerControl->callback = testTimerShouldChangeEnabledControl;

	ElektraIoTimerOperation * timerProbe = elektraIoTestSuiteUtilNewTimerOperation ();
	timerProbe->interval = TIMER_TEST_INTERVAL;
	timerProbe->enabled = 1;
	timerProbe->callback = testTimerShouldChangeEnabledProbe;

	ElektraIoInterface * binding = createBinding ();
	binding->addTimer (binding, timerControl);
	binding->addTimer (binding, timerProbe);

	testStop = stop;
	testUpdateEnabledControlCalled = TIMER_CHANGE_TIMES;
	testUpdateEnabledProbeCalled = 0;
	testUpdateEnabledTimerProbe = timerProbe;
	testUpdateEnabledBinding = binding;

	start ();

	succeed_if (testUpdateEnabledProbeCalled == 0, "timer callback was not disabled");
	succeed_if (testUpdateEnabledControlCalled == 0, "timout control callback was not called required amount of times");

	binding->removeTimer (timerControl);
	binding->removeTimer (timerProbe);
	binding->cleanup (binding);
	elektraFree (timerControl);
	elektraFree (timerProbe);
}

static void testTimerShouldChangeIntervalControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	testUpdateIntervalControlCalled++;

	// Change probe interval on first run, before probe was run
	if (testUpdateIntervalControlCalled == 1)
	{
		testUpdateIntervalTimerProbe->interval = TIMER_CHANGE_SECOND_INTERVAL;
		testUpdateIntervalBinding->updateTimer (testUpdateIntervalTimerProbe);
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
	ElektraIoTimerOperation * timerControl = elektraIoTestSuiteUtilNewTimerOperation ();
	timerControl->interval = TIMER_CHANGE_CONTROL_INTERVAL;
	timerControl->enabled = 1;
	timerControl->callback = testTimerShouldChangeIntervalControl;

	// Probe will just count and measure time
	ElektraIoTimerOperation * timerProbe = elektraIoTestSuiteUtilNewTimerOperation ();
	timerProbe->interval = TIMER_TEST_INTERVAL;
	timerProbe->enabled = 1;
	timerProbe->callback = testTimerShouldChangeIntervalProbe;

	ElektraIoInterface * binding = createBinding ();
	binding->addTimer (binding, timerControl);
	binding->addTimer (binding, timerProbe);

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

	binding->removeTimer (timerControl);
	binding->removeTimer (timerProbe);
	binding->cleanup (binding);
	elektraFree (timerControl);
	elektraFree (timerProbe);
}

static void testTimerShouldRemoveControl (ElektraIoTimerOperation * idleInfo ELEKTRA_UNUSED)
{
	testRemoveControlCalled--;

	// Disable probe timer on first run
	if (testRemoveControlCalled == TIMER_CHANGE_TIMES - 1)
	{
		testRemoveBinding->removeTimer (testRemoveTimerProbe);
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
	ElektraIoTimerOperation * timerControl = elektraIoTestSuiteUtilNewTimerOperation ();
	timerControl->interval = TIMER_CHANGE_CONTROL_INTERVAL;
	timerControl->enabled = 1;
	timerControl->callback = testTimerShouldRemoveControl;

	ElektraIoTimerOperation * timerProbe = elektraIoTestSuiteUtilNewTimerOperation ();
	timerProbe->interval = TIMER_TEST_INTERVAL;
	timerProbe->enabled = 1;
	timerProbe->callback = testTimerShouldRemoveProbe;

	ElektraIoInterface * binding = createBinding ();
	binding->addTimer (binding, timerControl);
	binding->addTimer (binding, timerProbe);

	testStop = stop;
	testRemoveControlCalled = TIMER_CHANGE_TIMES;
	testRemoveProbeCalled = 0;
	testRemoveTimerProbe = timerProbe;
	testRemoveBinding = binding;

	start ();

	succeed_if (testRemoveProbeCalled == 0, "timer callback was not removed");
	succeed_if (testRemoveControlCalled == 0, "timout control callback was not called required amount of times");

	binding->removeTimer (timerControl);
	if (testRemoveProbeCalled != 0)
	{
		binding->removeTimer (timerProbe);
	}
	binding->cleanup (binding);
	elektraFree (timerControl);
	elektraFree (timerProbe);
}

/**
 * Test timer functions of the IO-Binding returned by createBinding.
 * Requires the following operations: Timer
 *
 * @param createBinding binding creation function
 * @param start         starts IO operations
 * @param stop          stops IO operations
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

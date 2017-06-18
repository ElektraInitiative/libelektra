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
#include <unistd.h>

#include <tests.h>

#include "test.h"
#include <kdbio.h>

#define MIX_TIMER_INTERVAL 50
#define MIX_DIFF_WARNING_THRESHOLD 5
#define MIX_DIFF_ERROR_THRESHOLD (MIX_DIFF_WARNING_THRESHOLD * 100)

#define MIX_BUFFER_TESTDATA "T"
#define MIX_BUFFER_TESTDATA_LENGTH 1

ElektraIoTestSuiteStop testStop;

int testIdleNotStarveTimerTimerCalled;
int testIdleNotStarveTimerIdleCalled;
struct timespec testIdleNotStarveTimerTimeStarted;
struct timespec testIdleNotStarveTimerTimeCalled;

int testIdleNotStarveFdStep;
int testIdleNotStarveFdFdCalled;
int testIdleNotStarveFdIdleCalled;
int testIdleNotStarveFdWriteFd;
struct timespec testIdleNotStarveFdTimeReadable;
struct timespec testIdleNotStarveFdTimeWrite;

static void testMixIdleShouldNotStarveTimerTimer (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	elektraIoTestSuiteUtilGetCurrentTime (&testIdleNotStarveTimerTimeCalled);
	testIdleNotStarveTimerTimerCalled = 1;
	testStop ();
}

static void testMixIdleShouldNotStarveTimerIdle (ElektraIoIdleOperation * timerOp ELEKTRA_UNUSED)
{
	testIdleNotStarveTimerIdleCalled = 1;
}

static void testMixIdleShouldNotStarveTimer (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					     ElektraIoTestSuiteStop stop)
{
	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = MIX_TIMER_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testMixIdleShouldNotStarveTimerTimer;

	ElektraIoIdleOperation * idleOp = elektraIoTestSuiteUtilNewIdleOperation ();
	idleOp->enabled = 1;
	idleOp->callback = testMixIdleShouldNotStarveTimerIdle;

	ElektraIoInterface * binding = createBinding ();
	binding->addTimer (binding, timerOp);
	binding->addIdle (binding, idleOp);

	testStop = stop;
	testIdleNotStarveTimerTimerCalled = 0;
	testIdleNotStarveTimerIdleCalled = 0;

	elektraIoTestSuiteUtilGetCurrentTime (&testIdleNotStarveTimerTimeStarted);

	start ();

	succeed_if (testIdleNotStarveTimerTimerCalled, "timer callback was not called");
	succeed_if (testIdleNotStarveTimerIdleCalled, "idle callback was not called");

	long diff = elektraIoTestSuiteUtilGetTimeDifference (testIdleNotStarveTimerTimeStarted, testIdleNotStarveTimerTimeCalled);
	int deviation = labs (MIX_TIMER_INTERVAL - diff);
	if (deviation > MIX_DIFF_WARNING_THRESHOLD)
	{
		printf ("testMixIdleShouldNotStarveTimer (warning): measured %ldms, expected %dms - deviation %dms.\n", diff,
			MIX_TIMER_INTERVAL, deviation);
	}
	succeed_if (deviation <= MIX_DIFF_ERROR_THRESHOLD, "timer interval not within error threshold");

	binding->removeTimer (timerOp);
	binding->removeIdle (idleOp);
	binding->cleanup (binding);
	elektraFree (timerOp);
	elektraFree (idleOp);
}


static void testMixIdleShouldNotStarveFdFd (ElektraIoFdOperation * fdOp ELEKTRA_UNUSED, int flags ELEKTRA_UNUSED)
{
	elektraIoTestSuiteUtilGetCurrentTime (&testIdleNotStarveFdTimeReadable);
	succeed_if (testIdleNotStarveFdStep != 0, "fd called before data was written");

	testIdleNotStarveFdFdCalled = 1;
	testStop ();
}

static void testMixIdleShouldNotStarveFdControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	switch (testIdleNotStarveFdStep)
	{
	case 0:
		testIdleNotStarveFdStep++;
		succeed_if (write (testIdleNotStarveFdWriteFd, MIX_BUFFER_TESTDATA, MIX_BUFFER_TESTDATA_LENGTH) ==
				    MIX_BUFFER_TESTDATA_LENGTH,
			    "write failed");
		elektraIoTestSuiteUtilGetCurrentTime (&testIdleNotStarveFdTimeWrite);
		break;
	case 1:
		yield_error ("timeout; test failed");
		testStop ();
		break;
	}
}

static void testMixIdleShouldNotStarveFdIdle (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	testIdleNotStarveFdIdleCalled = 1;
}

static void testMixIdleShouldNotStarveFd (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					  ElektraIoTestSuiteStop stop)
{

	int fds[2];
	if (pipe (fds) == -1)
	{
		yield_error ("pipe() failed");
		return;
	}

	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = MIX_TIMER_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testMixIdleShouldNotStarveFdControl;

	ElektraIoFdOperation * fdOp = elektraIoTestSuiteUtilNewFdOperation ();
	fdOp->fd = fds[0];
	fdOp->flags = ELEKTRA_IO_READABLE; // gets changed by control timer
	fdOp->enabled = 1;
	fdOp->callback = testMixIdleShouldNotStarveFdFd;

	ElektraIoIdleOperation * idleOp = elektraIoTestSuiteUtilNewIdleOperation ();
	idleOp->enabled = 1;
	idleOp->callback = testMixIdleShouldNotStarveFdIdle;

	ElektraIoInterface * binding = createBinding ();
	binding->addFd (binding, fdOp);
	binding->addTimer (binding, timerOp);
	binding->addIdle (binding, idleOp);

	testIdleNotStarveFdWriteFd = fds[1];
	testIdleNotStarveFdStep = 0;
	testIdleNotStarveFdFdCalled = 0;
	testIdleNotStarveFdIdleCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testIdleNotStarveFdFdCalled, "fd callback was not called");
	succeed_if (testIdleNotStarveFdIdleCalled, "idle callback was not called");

	binding->removeFd (fdOp);
	binding->removeTimer (timerOp);
	binding->removeIdle (idleOp);
	binding->cleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	elektraFree (idleOp);
	close (fds[0]);
	close (fds[1]);
}

/**
 * Test mixed requirements of the IO-Binding returned by createBinding.
 * Requires the following operations: Fd, Timer, Idle
 *
 * @param createBinding binding creation function
 * @param start         starts IO operations
 * @param stop          stops IO operations
 */
void elektraIoTestSuiteMix (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	printf ("test mix\n");

	testMixIdleShouldNotStarveTimer (createBinding, start, stop);

	testMixIdleShouldNotStarveFd (createBinding, start, stop);
}

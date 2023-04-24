/**
 * @file
 *
 * @brief Tests for I/O bindings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * See test_fd.c for why `pipe()` is used.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <tests.h>

#include "./test.h"
#include <elektra/io/api.h>
#include <internal/io/test.h>

#define MIX_TIMER_INTERVAL 100
#define MIX_DIFF_WARNING_THRESHOLD 5
#define MIX_DIFF_ERROR_THRESHOLD (MIX_DIFF_WARNING_THRESHOLD * 100)

#define MIX_BUFFER_TESTDATA "T"
#define MIX_BUFFER_TESTDATA_LENGTH 1

// Indices for array returned by pipe()
#define MIX_READ_END 0
#define MIX_WRITE_END 1

static ElektraIoTestSuiteStop testStop;

static int testIdleNotStarveTimerTimerCalled;
static int testIdleNotStarveTimerIdleCalled;
static struct timespec testIdleNotStarveTimerTimeStarted;
static struct timespec testIdleNotStarveTimerTimeCalled;

static int testIdleNotStarveFdStep;
static int testIdleNotStarveFdFdCalled;
static int testIdleNotStarveFdIdleCalled;
static int testIdleNotStarveFdWriteFd;
static struct timespec testIdleNotStarveFdTimeReadable;
static struct timespec testIdleNotStarveFdTimeWrite;

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
	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (MIX_TIMER_INTERVAL, 1, testMixIdleShouldNotStarveTimerTimer, NULL);

	ElektraIoIdleOperation * idleOp = elektraIoNewIdleOperation (1, testMixIdleShouldNotStarveTimerIdle, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddTimer (binding, timerOp);
	elektraIoBindingAddIdle (binding, idleOp);

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

	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingRemoveIdle (idleOp);
	elektraIoBindingCleanup (binding);
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

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (MIX_TIMER_INTERVAL, 1, testMixIdleShouldNotStarveFdControl, NULL);

	ElektraIoFdOperation * fdOp = elektraIoNewFdOperation (fds[MIX_READ_END],
							       ELEKTRA_IO_READABLE, // gets changed by control timer
							       1, testMixIdleShouldNotStarveFdFd, NULL);

	ElektraIoIdleOperation * idleOp = elektraIoNewIdleOperation (1, testMixIdleShouldNotStarveFdIdle, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddFd (binding, fdOp);
	elektraIoBindingAddTimer (binding, timerOp);
	elektraIoBindingAddIdle (binding, idleOp);

	testIdleNotStarveFdWriteFd = fds[1];
	testIdleNotStarveFdStep = 0;
	testIdleNotStarveFdFdCalled = 0;
	testIdleNotStarveFdIdleCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testIdleNotStarveFdFdCalled, "fd callback was not called");
	succeed_if (testIdleNotStarveFdIdleCalled, "idle callback was not called");

	elektraIoBindingRemoveFd (fdOp);
	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingRemoveIdle (idleOp);
	elektraIoBindingCleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	elektraFree (idleOp);
	close (fds[0]);
	close (fds[1]);
}

/**
 * Test mixed requirements of the I/O binding returned by createBinding.
 * Requires the following operations: Fd, Timer, Idle
 *
 * @param createBinding binding creation function
 * @param start         starts I/O operations
 * @param stop          stops I/O operations
 */
void elektraIoTestSuiteMix (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	printf ("test mix\n");

	testMixIdleShouldNotStarveTimer (createBinding, start, stop);

	testMixIdleShouldNotStarveFd (createBinding, start, stop);
}

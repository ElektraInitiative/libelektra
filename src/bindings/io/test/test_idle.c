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

#include <internal/utility/old_helper.h>
#include <tests.h>

#include "test.h"
#include <elektra/io/api.h>
#include <internal/io/test.h>

#define IDLE_TEST_INTERVAL 1
#define IDLE_TEST_CONTROL_TIMES 3
#define IDLE_DIFF_WARNING_THRESHOLD 5
#define IDLE_DIFF_ERROR_THRESHOLD (IDLE_DIFF_WARNING_THRESHOLD * 100)

static ElektraIoTestSuiteStop testStop;

static int testCallbackCalled;
static struct timespec testCallbackTimeStarted;
static struct timespec testCallbackTimeCalled;

static int testUpdateEnabledControlCalled;
static int testUpdateEnabledProbeCalled;
static ElektraIoIdleOperation * testUpdateEnabledIdleProbe;
static ElektraIoInterface * testUpdateEnabledBinding;

static int testRemoveControlCalled;
static int testRemoveProbeCalled;
static ElektraIoIdleOperation * testRemoveIdleProbe;
static ElektraIoInterface * testRemoveBinding;

static void testIdleBasicsCallback (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	yield_error ("should not be called");
}

static void testIdleBasics (ElektraIoTestSuiteCreateBinding createBinding)
{
	ElektraIoIdleOperation * idleOp = elektraIoNewIdleOperation (0, testIdleBasicsCallback, NULL);

	ElektraIoInterface * binding = createBinding ();
	succeed_if (elektraIoBindingAddIdle (binding, idleOp), "addIdle did not succeed");
	succeed_if (elektraIoBindingAddIdle (binding, idleOp) == 0, "addIdle: should not be able to reassign operation to a binding");

	elektraIoIdleSetEnabled (idleOp, 1);
	succeed_if (elektraIoBindingUpdateIdle (idleOp), "updateIdle did not succeed");

	succeed_if (elektraIoBindingRemoveIdle (idleOp), "removeIdle did not succeed");

	succeed_if (elektraIoBindingAddIdle (binding, idleOp), "addIdle: should be able to assign operation after removal");
	succeed_if (elektraIoBindingRemoveIdle (idleOp), "removeIdle did not succeed");
	elektraIoBindingCleanup (binding);
	elektraFree (idleOp);
}

static void testIdleShouldCallbackImmediatelyProbe (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	testCallbackCalled = 1;
	elektraIoTestSuiteUtilGetCurrentTime (&testCallbackTimeCalled);
	testStop ();
}

static void testIdleShouldCallbackImmediately (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					       ElektraIoTestSuiteStop stop)
{
	ElektraIoIdleOperation * idleOp = elektraIoNewIdleOperation (1, testIdleShouldCallbackImmediatelyProbe, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddIdle (binding, idleOp);

	testStop = stop;
	testCallbackCalled = 0;

	elektraIoTestSuiteUtilGetCurrentTime (&testCallbackTimeStarted);

	start ();

	succeed_if (testCallbackCalled, "callback was not called");

	long diff = elektraIoTestSuiteUtilGetTimeDifference (testCallbackTimeStarted, testCallbackTimeCalled);
	int deviation = labs (IDLE_TEST_INTERVAL - diff);
	if (deviation > IDLE_DIFF_WARNING_THRESHOLD)
	{
		printf ("testIdleShouldCallbackImmediately (warning): measured %ldms, expected %dms - deviation %dms.\n", diff,
			IDLE_TEST_INTERVAL, deviation);
	}
	succeed_if (deviation <= IDLE_DIFF_ERROR_THRESHOLD, "idle timing not within error threshold");

	elektraIoBindingRemoveIdle (idleOp);
	elektraIoBindingCleanup (binding);
	elektraFree (idleOp);
}

static void testIdleShouldUpdateEnabledControl (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	testUpdateEnabledControlCalled++;

	// Disable probe operation on second run
	if (testUpdateEnabledProbeCalled == IDLE_TEST_CONTROL_TIMES - 1)
	{
		elektraIoIdleSetEnabled (testUpdateEnabledIdleProbe, 0);
		elektraIoBindingUpdateIdle (testUpdateEnabledIdleProbe);
	}

	// Stop test when control limit was reached or probe was called twice
	if (testUpdateEnabledControlCalled == IDLE_TEST_CONTROL_TIMES || testUpdateEnabledProbeCalled > 2)
	{
		testStop ();
	}
}

static void testIdleShouldUpdateEnabledProbe (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	testUpdateEnabledProbeCalled++;
}

static void testIdleShouldUpdateEnabled (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					 ElektraIoTestSuiteStop stop)
{
	ElektraIoIdleOperation * idleControl = elektraIoNewIdleOperation (1, testIdleShouldUpdateEnabledControl, NULL);

	ElektraIoIdleOperation * idleProbe = elektraIoNewIdleOperation (1, testIdleShouldUpdateEnabledProbe, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddIdle (binding, idleControl);
	elektraIoBindingAddIdle (binding, idleProbe);

	testStop = stop;
	testUpdateEnabledControlCalled = 0; // IDLE_TEST_CONTROL_TIMES;
	testUpdateEnabledProbeCalled = 0;
	testUpdateEnabledIdleProbe = idleProbe;
	testUpdateEnabledBinding = binding;

	start ();

	succeed_if (testUpdateEnabledProbeCalled > 0 && testUpdateEnabledProbeCalled <= 2, "idle callback was not disabled");
	succeed_if (testUpdateEnabledControlCalled == IDLE_TEST_CONTROL_TIMES,
		    "idle control callback was not called required amount of times");

	elektraIoBindingRemoveIdle (idleControl);
	elektraIoBindingRemoveIdle (idleProbe);
	elektraIoBindingCleanup (binding);
	elektraFree (idleControl);
	elektraFree (idleProbe);
}

static void testIdleShouldRemoveControl (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	testRemoveControlCalled++;

	// Remove probe operation on first run
	if (testRemoveControlCalled == IDLE_TEST_CONTROL_TIMES - 1)
	{
		elektraIoBindingRemoveIdle (testRemoveIdleProbe);
	}

	if (testRemoveControlCalled == IDLE_TEST_CONTROL_TIMES || testRemoveProbeCalled > 2)
	{
		testStop ();
	}
}

static void testIdleShouldRemoveProbe (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	testRemoveProbeCalled++;
}

static void testIdleShouldRemove (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	ElektraIoIdleOperation * idleControl = elektraIoNewIdleOperation (1, testIdleShouldRemoveControl, NULL);

	ElektraIoIdleOperation * idleProbe = elektraIoNewIdleOperation (1, testIdleShouldRemoveProbe, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddIdle (binding, idleControl);
	elektraIoBindingAddIdle (binding, idleProbe);

	testStop = stop;
	testRemoveControlCalled = 0;
	testRemoveProbeCalled = 0;
	testRemoveIdleProbe = idleProbe;
	testRemoveBinding = binding;

	start ();

	succeed_if (testRemoveProbeCalled > 0 && testRemoveProbeCalled <= 2, "idle callback was not removed");

	succeed_if (testRemoveControlCalled == IDLE_TEST_CONTROL_TIMES, "idle control callback was not called required amount of times");

	elektraIoBindingRemoveIdle (idleControl);
	elektraIoBindingCleanup (binding);
	elektraFree (idleControl);
	elektraFree (idleProbe);
}


/**
 * Test idle functions of the I/O binding returned by createBinding.
 * Requires the following operations: Idle
 *
 * @param createBinding binding creation function
 * @param start         starts I/O operations
 * @param stop          stops I/O operations
 */
void elektraIoTestSuiteIdle (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	printf ("test idle\n");

	testIdleBasics (createBinding);

	testIdleShouldCallbackImmediately (createBinding, start, stop);

	testIdleShouldUpdateEnabled (createBinding, start, stop);

	testIdleShouldRemove (createBinding, start, stop);
}

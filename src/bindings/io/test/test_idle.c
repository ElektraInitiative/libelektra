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

#define IDLE_TEST_INTERVAL 1
#define IDLE_TEST_CONTROL_TIMES 2
#define IDLE_DIFF_WARNING_THRESHOLD 5
#define IDLE_DIFF_ERROR_THRESHOLD (IDLE_DIFF_WARNING_THRESHOLD * 100)

ElektraIoTestSuiteStop testStop;

int testCallbackCalled;
struct timespec testCallbackTimeStarted;
struct timespec testCallbackTimeCalled;

int testUpdateEnabledControlCalled;
int testUpdateEnabledProbeCalled;
ElektraIoIdleOperation * testUpdateEnabledIdleProbe;
ElektraIoInterface * testUpdateEnabledBinding;

int testRemoveControlCalled;
int testRemoveProbeCalled;
ElektraIoIdleOperation * testRemoveIdleProbe;
ElektraIoInterface * testRemoveBinding;

static void testIdleBasicsCallback (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	yield_error ("should not be called");
}

static void testIdleBasics (ElektraIoTestSuiteCreateBinding createBinding)
{
	ElektraIoIdleOperation * idleOp = elektraIoTestSuiteUtilNewIdleOperation ();
	idleOp->enabled = 0;
	idleOp->callback = testIdleBasicsCallback;

	ElektraIoInterface * binding = createBinding ();
	succeed_if (binding->addIdle (binding, idleOp) == 0, "addIdle did not return 0");
	succeed_if (idleOp->binding == binding, "binding was not set");

	idleOp->enabled = 1;
	succeed_if (binding->updateIdle (idleOp) == 0, "updateIdle did not return 0");

	succeed_if (binding->removeIdle (idleOp) == 0, "removeIdle did not return 0");
	binding->cleanup (binding);
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
	ElektraIoIdleOperation * idleOp = elektraIoTestSuiteUtilNewIdleOperation ();
	idleOp->enabled = 1;
	idleOp->callback = testIdleShouldCallbackImmediatelyProbe;

	ElektraIoInterface * binding = createBinding ();
	binding->addIdle (binding, idleOp);

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

	binding->removeIdle (idleOp);
	binding->cleanup (binding);
	elektraFree (idleOp);
}

static void testIdleShouldUpdateEnabledControl (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	testUpdateEnabledControlCalled--;

	// Disable probe operation on first run
	if (testUpdateEnabledControlCalled == IDLE_TEST_CONTROL_TIMES - 1)
	{
		testUpdateEnabledIdleProbe->enabled = 0;
		testUpdateEnabledBinding->updateIdle (testUpdateEnabledIdleProbe);
	}

	if (testUpdateEnabledControlCalled == 0 || testUpdateEnabledProbeCalled > 1)
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
	ElektraIoIdleOperation * idleControl = elektraIoTestSuiteUtilNewIdleOperation ();
	idleControl->enabled = 1;
	idleControl->callback = testIdleShouldUpdateEnabledControl;

	ElektraIoIdleOperation * idleProbe = elektraIoTestSuiteUtilNewIdleOperation ();
	idleProbe->enabled = 1;
	idleProbe->callback = testIdleShouldUpdateEnabledProbe;

	ElektraIoInterface * binding = createBinding ();
	binding->addIdle (binding, idleControl);
	binding->addIdle (binding, idleProbe);

	testStop = stop;
	testUpdateEnabledControlCalled = IDLE_TEST_CONTROL_TIMES;
	testUpdateEnabledProbeCalled = 0;
	testUpdateEnabledIdleProbe = idleProbe;
	testUpdateEnabledBinding = binding;

	start ();

	succeed_if (testUpdateEnabledProbeCalled == 1, "idle callback was not disabled");
	succeed_if (testUpdateEnabledControlCalled == 0, "idle control callback was not called required amount of times");

	binding->removeIdle (idleControl);
	binding->removeIdle (idleProbe);
	binding->cleanup (binding);
	elektraFree (idleControl);
	elektraFree (idleProbe);
}

static void testIdleShouldRemoveControl (ElektraIoIdleOperation * idleOp ELEKTRA_UNUSED)
{
	testRemoveControlCalled--;

	// Remove probe operation on first run
	if (testRemoveControlCalled == IDLE_TEST_CONTROL_TIMES - 1)
	{
		testRemoveBinding->removeIdle (testRemoveIdleProbe);
	}

	if (testRemoveControlCalled == 0 || testRemoveProbeCalled > 1)
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
	ElektraIoIdleOperation * idleControl = elektraIoTestSuiteUtilNewIdleOperation ();
	idleControl->enabled = 1;
	idleControl->callback = testIdleShouldRemoveControl;

	ElektraIoIdleOperation * idleProbe = elektraIoTestSuiteUtilNewIdleOperation ();
	idleProbe->enabled = 1;
	idleProbe->callback = testIdleShouldRemoveProbe;

	ElektraIoInterface * binding = createBinding ();
	binding->addIdle (binding, idleControl);
	binding->addIdle (binding, idleProbe);

	testStop = stop;
	testRemoveControlCalled = IDLE_TEST_CONTROL_TIMES;
	testRemoveProbeCalled = 0;
	testRemoveIdleProbe = idleProbe;
	testRemoveBinding = binding;

	start ();

	succeed_if (testRemoveProbeCalled == 1, "idle callback was not removed");

	succeed_if (testRemoveControlCalled == 0, "idle control callback was not called required amount of times");

	binding->removeIdle (idleControl);
	if (testRemoveProbeCalled != 1)
	{
		binding->removeIdle (idleProbe);
	}
	binding->cleanup (binding);
	elektraFree (idleControl);
	elektraFree (idleProbe);
}


/**
 * Test idle functions of the IO-Binding returned by createBinding.
 * Requires the following operations: Idle
 *
 * @param createBinding binding creation function
 * @param start         starts IO operations
 * @param stop          stops IO operations
 */
void elektraIoTestSuiteIdle (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	printf ("test idle\n");

	testIdleBasics (createBinding);

	testIdleShouldCallbackImmediately (createBinding, start, stop);

	testIdleShouldUpdateEnabled (createBinding, start, stop);

	testIdleShouldRemove (createBinding, start, stop);
}

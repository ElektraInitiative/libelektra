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

#define FD_CONTROL_INTERVAL 50

#define FD_BUFFER_TESTDATA "T"
#define FD_BUFFER_TESTDATA_LENGTH 1

ElektraIoTestSuiteStop testStop;

int testSignalWritableCalled;

int testSignalReadableCalled;

int testUpdateEnabledCalled;
int testUpdateEnabledStep;
ElektraIoInterface * testUpdateEnabledBinding;
ElektraIoFdOperation * testUpdateEnabledFdOp;

int testUpdateFlagsCalled;
int testUpdateFlagsStep;
ElektraIoInterface * testUpdateFlagsBinding;
ElektraIoFdOperation * testUpdateFlagsFdOp;

int testRemoveCalled;
int testRemoveStep;
ElektraIoInterface * testRemoveBinding;
ElektraIoFdOperation * testRemoveFdOp;

static void testFdBasicsCallback (ElektraIoFdOperation * fdOp ELEKTRA_UNUSED, int flags ELEKTRA_UNUSED)
{
	yield_error ("should not be called");
}

static void testFdBasics (ElektraIoTestSuiteCreateBinding createBinding)
{
	int fds[2];
	if (pipe (fds) == -1)
	{
		yield_error ("pipe() failed");
		return;
	}

	ElektraIoFdOperation * fdOp = elektraIoTestSuiteUtilNewFdOperation ();
	fdOp->fd = fds[0];
	fdOp->flags = ELEKTRA_IO_WRITABLE;
	fdOp->enabled = 0;
	fdOp->callback = testFdBasicsCallback;

	ElektraIoInterface * binding = createBinding ();
	succeed_if (binding->addFd (binding, fdOp) == 0, "addFd did not return 0");
	succeed_if (fdOp->binding == binding, "binding was not set");

	fdOp->enabled = 1;
	succeed_if (binding->updateFd (fdOp) == 0, "updateFd did not return 0");

	succeed_if (binding->removeFd (fdOp) == 0, "removeFd did not return 0");
	binding->cleanup (binding);
	elektraFree (fdOp);
}

static void testFdShouldSignalXControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	yield_error ("timeout; test failed");
	testStop ();
}

static void testFdShouldSignalWritableProbe (ElektraIoFdOperation * fdOp ELEKTRA_UNUSED, int flags)
{
	succeed_if (flags & ELEKTRA_IO_WRITABLE, "flags does not contain ELEKTRA_IO_WRITABLE");
	testSignalWritableCalled = 1;
	testStop ();
}

static void testFdShouldSignalWritable (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					ElektraIoTestSuiteStop stop)
{

	int fds[2];
	if (pipe (fds) == -1)
	{
		yield_error ("pipe() failed");
		return;
	}

	ElektraIoFdOperation * fdOp = elektraIoTestSuiteUtilNewFdOperation ();
	fdOp->fd = fds[1];
	fdOp->flags = ELEKTRA_IO_WRITABLE;
	fdOp->enabled = 1;
	fdOp->callback = testFdShouldSignalWritableProbe;

	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = FD_CONTROL_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testFdShouldSignalXControl;

	ElektraIoInterface * binding = createBinding ();
	binding->addFd (binding, fdOp);
	binding->addTimer (binding, timerOp);

	testSignalWritableCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testSignalWritableCalled, "callback was not called");

	binding->removeFd (fdOp);
	binding->removeTimer (timerOp);
	binding->cleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[0]);
	close (fds[1]);
}

static void testFdShouldSignalReadableProbe (ElektraIoFdOperation * fdOp, int flags)
{
	char buffer[FD_BUFFER_TESTDATA_LENGTH];
	succeed_if (read (fdOp->fd, &buffer, FD_BUFFER_TESTDATA_LENGTH) == FD_BUFFER_TESTDATA_LENGTH, "read failed");
	succeed_if (strncmp (buffer, FD_BUFFER_TESTDATA, FD_BUFFER_TESTDATA_LENGTH) == 0, "did not read correct data");

	succeed_if (flags & ELEKTRA_IO_READABLE, "flags does not contain ELEKTRA_IO_READABLE");
	testSignalReadableCalled = 1;
	testStop ();
}

static void testFdShouldSignalReadable (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
					ElektraIoTestSuiteStop stop)
{

	int fds[2];
	if (pipe (fds) == -1)
	{
		yield_error ("pipe() failed");
		return;
	}
	char * buffer = FD_BUFFER_TESTDATA;
	succeed_if (write (fds[1], buffer, FD_BUFFER_TESTDATA_LENGTH) == FD_BUFFER_TESTDATA_LENGTH, "write failed");

	ElektraIoFdOperation * fdOp = elektraIoTestSuiteUtilNewFdOperation ();
	fdOp->fd = fds[0];
	fdOp->flags = ELEKTRA_IO_READABLE;
	fdOp->enabled = 1;
	fdOp->callback = testFdShouldSignalReadableProbe;

	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = FD_CONTROL_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testFdShouldSignalXControl;

	ElektraIoInterface * binding = createBinding ();
	binding->addFd (binding, fdOp);
	binding->addTimer (binding, timerOp);

	testSignalReadableCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testSignalReadableCalled, "callback was not called");

	binding->removeFd (fdOp);
	binding->removeTimer (timerOp);
	binding->cleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[0]);
	close (fds[1]);
}

static void testFdShouldUpdateEnabledControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	switch (testUpdateEnabledStep)
	{
	case 0:
		testUpdateEnabledFdOp->enabled = 1;
		testUpdateEnabledBinding->updateFd (testUpdateEnabledFdOp);
		break;
	case 1:
		yield_error ("timeout; test failed");
		testStop ();
		break;
	}
	testUpdateEnabledStep++;
}

static void testFdShouldUpdateEnabledProbe (ElektraIoFdOperation * fdOp ELEKTRA_UNUSED, int flags ELEKTRA_UNUSED)
{
	succeed_if (testUpdateEnabledStep != 0, "callback called before enabeld was updated");
	testUpdateEnabledCalled = 1;
	testStop ();
}

static void testFdShouldUpdateEnabled (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
				       ElektraIoTestSuiteStop stop)
{

	int fds[2];
	if (pipe (fds) == -1)
	{
		yield_error ("pipe() failed");
		return;
	}

	ElektraIoFdOperation * fdOp = elektraIoTestSuiteUtilNewFdOperation ();
	fdOp->fd = fds[1];
	fdOp->flags = ELEKTRA_IO_WRITABLE;
	fdOp->enabled = 0; // gets enabled by control timer
	fdOp->callback = testFdShouldUpdateEnabledProbe;

	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = FD_CONTROL_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testFdShouldUpdateEnabledControl;

	ElektraIoInterface * binding = createBinding ();
	binding->addFd (binding, fdOp);
	binding->addTimer (binding, timerOp);

	testUpdateEnabledStep = 0;
	testUpdateEnabledBinding = binding;
	testUpdateEnabledFdOp = fdOp;
	testUpdateEnabledCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testUpdateEnabledCalled, "callback was not called");

	binding->removeFd (fdOp);
	binding->removeTimer (timerOp);
	binding->cleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[0]);
	close (fds[1]);
}

static void testFdShouldUpdateFlagsControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	switch (testUpdateFlagsStep)
	{
	case 0:
		testUpdateFlagsFdOp->flags = ELEKTRA_IO_WRITABLE;
		testUpdateFlagsBinding->updateFd (testUpdateFlagsFdOp);
		break;
	case 1:
		yield_error ("timeout; test failed");
		testStop ();
		break;
	}
	testUpdateFlagsStep++;
}

static void testFdShouldUpdateFlagsProbe (ElektraIoFdOperation * fdOp ELEKTRA_UNUSED, int flags ELEKTRA_UNUSED)
{
	succeed_if (testUpdateFlagsStep != 0, "callback called before flags were updated");
	testUpdateFlagsCalled = 1;
	testStop ();
}

static void testFdShouldUpdateFlags (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start,
				     ElektraIoTestSuiteStop stop)
{

	int fds[2];
	if (pipe (fds) == -1)
	{
		yield_error ("pipe() failed");
		return;
	}

	ElektraIoFdOperation * fdOp = elektraIoTestSuiteUtilNewFdOperation ();
	fdOp->fd = fds[1];
	fdOp->flags = ELEKTRA_IO_READABLE; // gets changed by control timer
	fdOp->enabled = 1;
	fdOp->callback = testFdShouldUpdateFlagsProbe;

	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = FD_CONTROL_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testFdShouldUpdateFlagsControl;

	ElektraIoInterface * binding = createBinding ();
	binding->addFd (binding, fdOp);
	binding->addTimer (binding, timerOp);

	testUpdateFlagsStep = 0;
	testUpdateFlagsBinding = binding;
	testUpdateFlagsFdOp = fdOp;
	testUpdateFlagsCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testUpdateFlagsCalled, "callback was not called");

	binding->removeFd (fdOp);
	binding->removeTimer (timerOp);
	binding->cleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[0]);
	close (fds[1]);
}

static void testFdShouldRemoveControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	switch (testRemoveStep)
	{
	case 0:
		testRemoveBinding->removeFd (testRemoveFdOp);
		char * buffer = "this should not fire the callback";
		succeed_if (write (testRemoveFdOp->fd, buffer, FD_BUFFER_TESTDATA_LENGTH) == FD_BUFFER_TESTDATA_LENGTH, "write failed");
		break;
	case 1:
		testStop ();
		break;
	}
	testRemoveStep++;
}

static void testFdShouldRemoveProbe (ElektraIoFdOperation * fdOp ELEKTRA_UNUSED, int flags ELEKTRA_UNUSED)
{
	succeed_if (testRemoveStep == 0, "callback called after fd was removed");
	testRemoveCalled = 1;
	testStop ();
}

static void testFdShouldRemove (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	int fds[2];
	if (pipe (fds) == -1)
	{
		yield_error ("pipe() failed");
		return;
	}

	ElektraIoFdOperation * fdOp = elektraIoTestSuiteUtilNewFdOperation ();
	fdOp->fd = fds[1];
	fdOp->flags = ELEKTRA_IO_READABLE; // gets changed by control timer
	fdOp->enabled = 1;
	fdOp->callback = testFdShouldRemoveProbe;

	ElektraIoTimerOperation * timerOp = elektraIoTestSuiteUtilNewTimerOperation ();
	timerOp->interval = FD_CONTROL_INTERVAL;
	timerOp->enabled = 1;
	timerOp->callback = testFdShouldRemoveControl;

	ElektraIoInterface * binding = createBinding ();
	binding->addFd (binding, fdOp);
	binding->addTimer (binding, timerOp);

	testRemoveStep = 0;
	testRemoveBinding = binding;
	testRemoveFdOp = fdOp;
	testRemoveCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testRemoveCalled == 0, "callback was called");

	if (testRemoveStep == 0)
	{
		binding->removeFd (fdOp);
	}
	binding->removeTimer (timerOp);
	binding->cleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[0]);
	close (fds[1]);
}

/**
 * Test fd functions of the IO-Binding returned by createBinding.
 * Requires the following operations: Fd, Timer
 *
 * @param createBinding binding creation function
 * @param start         starts IO operations
 * @param stop          stops IO operations
 */
void elektraIoTestSuiteFd (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	printf ("test file descriptor\n");

	testFdBasics (createBinding);

	testFdShouldSignalWritable (createBinding, start, stop);

	testFdShouldSignalReadable (createBinding, start, stop);

	testFdShouldUpdateEnabled (createBinding, start, stop);

	testFdShouldUpdateFlags (createBinding, start, stop);

	testFdShouldRemove (createBinding, start, stop);
}

/**
 * @file
 *
 * @brief Tests for I/O bindings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * In these tests `pipe()` is used to create file descriptors that can be used
 * for testing readability or writability. Additionally `pipe()` is POSIX
 * compliant making the tests more portable.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <tests.h>

#include "./test.h"
#include <elektra/io/api.h>
#include <internal/io/test.h>
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>

#define FD_CONTROL_INTERVAL 200

#define FD_BUFFER_TESTDATA "T"
#define FD_BUFFER_TESTDATA_LENGTH 1

// Indices for array returned by pipe()
#define FD_READ_END 0
#define FD_WRITE_END 1

static ElektraIoTestSuiteStop testStop;

static int testSignalWritableCalled;

static int testSignalReadableCalled;

static int testUpdateEnabledCalled;
static int testUpdateEnabledStep;
static ElektraIoInterface * testUpdateEnabledBinding;
static ElektraIoFdOperation * testUpdateEnabledFdOp;

static int testUpdateFlagsCalled;
static int testUpdateFlagsStep;
static ElektraIoInterface * testUpdateFlagsBinding;
static ElektraIoFdOperation * testUpdateFlagsFdOp;

static int testRemoveCalled;
static int testRemoveStep;
static ElektraIoInterface * testRemoveBinding;
static ElektraIoFdOperation * testRemoveFdOp;

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

	ElektraIoFdOperation * fdOp = elektraIoNewFdOperation (fds[FD_READ_END], ELEKTRA_IO_WRITABLE, 0, testFdBasicsCallback, NULL);

	ElektraIoInterface * binding = createBinding ();
	succeed_if (elektraIoBindingAddFd (binding, fdOp), "addFd did not succeed");
	succeed_if (elektraIoBindingAddFd (binding, fdOp) == 0, "addFd: should not be able to reassign operation to a binding");

	elektraIoFdSetEnabled (fdOp, 1);
	succeed_if (elektraIoBindingUpdateFd (fdOp), "updateFd did not succeed");

	succeed_if (elektraIoBindingRemoveFd (fdOp), "removeFd did not succeed");

	succeed_if (elektraIoBindingAddFd (binding, fdOp), "addFd: should be able to assign operation after removal");
	succeed_if (elektraIoBindingRemoveFd (fdOp), "removeFd did not succeed");
	elektraIoBindingCleanup (binding);
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

	ElektraIoFdOperation * fdOp =
		elektraIoNewFdOperation (fds[FD_WRITE_END], ELEKTRA_IO_WRITABLE, 1, testFdShouldSignalWritableProbe, NULL);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (FD_CONTROL_INTERVAL, 1, testFdShouldSignalXControl, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddFd (binding, fdOp);
	elektraIoBindingAddTimer (binding, timerOp);

	testSignalWritableCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testSignalWritableCalled, "callback was not called");

	elektraIoBindingRemoveFd (fdOp);
	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingCleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[FD_READ_END]);
	close (fds[FD_WRITE_END]);
}

static void testFdShouldSignalReadableProbe (ElektraIoFdOperation * fdOp, int flags)
{
	char buffer[FD_BUFFER_TESTDATA_LENGTH];
	succeed_if (read (elektraIoFdGetFd (fdOp), &buffer, FD_BUFFER_TESTDATA_LENGTH) == FD_BUFFER_TESTDATA_LENGTH, "read failed");
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
	succeed_if (write (fds[FD_WRITE_END], buffer, FD_BUFFER_TESTDATA_LENGTH) == FD_BUFFER_TESTDATA_LENGTH, "write failed");

	ElektraIoFdOperation * fdOp =
		elektraIoNewFdOperation (fds[FD_READ_END], ELEKTRA_IO_READABLE, 1, testFdShouldSignalReadableProbe, NULL);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (FD_CONTROL_INTERVAL, 1, testFdShouldSignalXControl, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddFd (binding, fdOp);
	elektraIoBindingAddTimer (binding, timerOp);

	testSignalReadableCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testSignalReadableCalled, "callback was not called");

	elektraIoBindingRemoveFd (fdOp);
	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingCleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[FD_READ_END]);
	close (fds[FD_WRITE_END]);
}

static void testFdShouldUpdateEnabledControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	switch (testUpdateEnabledStep)
	{
	case 0:
		elektraIoFdSetEnabled (testUpdateEnabledFdOp, 1);
		elektraIoBindingUpdateFd (testUpdateEnabledFdOp);
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

	ElektraIoFdOperation * fdOp = elektraIoNewFdOperation (fds[FD_WRITE_END], ELEKTRA_IO_WRITABLE,
							       0, // gets enabled by control timer
							       testFdShouldUpdateEnabledProbe, NULL);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (FD_CONTROL_INTERVAL, 1, testFdShouldUpdateEnabledControl, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddFd (binding, fdOp);
	elektraIoBindingAddTimer (binding, timerOp);

	testUpdateEnabledStep = 0;
	testUpdateEnabledBinding = binding;
	testUpdateEnabledFdOp = fdOp;
	testUpdateEnabledCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testUpdateEnabledCalled, "callback was not called");

	elektraIoBindingRemoveFd (fdOp);
	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingCleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[FD_READ_END]);
	close (fds[FD_WRITE_END]);
}

static void testFdShouldUpdateFlagsControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	switch (testUpdateFlagsStep)
	{
	case 0:
		elektraIoFdSetFlags (testUpdateFlagsFdOp, ELEKTRA_IO_WRITABLE);
		elektraIoBindingUpdateFd (testUpdateFlagsFdOp);
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

	ElektraIoFdOperation * fdOp = elektraIoNewFdOperation (fds[FD_WRITE_END],
							       ELEKTRA_IO_READABLE, // gets changed by control timer
							       1, testFdShouldUpdateFlagsProbe, NULL);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (FD_CONTROL_INTERVAL, 1, testFdShouldUpdateFlagsControl, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddFd (binding, fdOp);
	elektraIoBindingAddTimer (binding, timerOp);

	testUpdateFlagsStep = 0;
	testUpdateFlagsBinding = binding;
	testUpdateFlagsFdOp = fdOp;
	testUpdateFlagsCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testUpdateFlagsCalled, "callback was not called");

	elektraIoBindingRemoveFd (fdOp);
	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingCleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[FD_READ_END]);
	close (fds[FD_WRITE_END]);
}

static void testFdShouldRemoveControl (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	switch (testRemoveStep)
	{
	case 0:
		elektraIoBindingRemoveFd (testRemoveFdOp);
		char * buffer = "this should not fire the callback";
		succeed_if (write (elektraIoFdGetFd (testRemoveFdOp), buffer, FD_BUFFER_TESTDATA_LENGTH) == FD_BUFFER_TESTDATA_LENGTH,
			    "write failed");
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

	ElektraIoFdOperation * fdOp = elektraIoNewFdOperation (fds[FD_WRITE_END],
							       ELEKTRA_IO_READABLE, // gets changed by control timer
							       1, testFdShouldRemoveProbe, NULL);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (FD_CONTROL_INTERVAL, 1, testFdShouldRemoveControl, NULL);

	ElektraIoInterface * binding = createBinding ();
	elektraIoBindingAddFd (binding, fdOp);
	elektraIoBindingAddTimer (binding, timerOp);

	testRemoveStep = 0;
	testRemoveBinding = binding;
	testRemoveFdOp = fdOp;
	testRemoveCalled = 0;
	testStop = stop;

	start ();

	succeed_if (testRemoveCalled == 0, "callback was called");

	if (testRemoveStep == 0)
	{
		elektraIoBindingRemoveFd (fdOp);
	}
	elektraIoBindingRemoveTimer (timerOp);
	elektraIoBindingCleanup (binding);
	elektraFree (fdOp);
	elektraFree (timerOp);
	close (fds[FD_READ_END]);
	close (fds[FD_WRITE_END]);
}

/**
 * Test fd functions of the I/O binding returned by createBinding.
 * Requires the following operations: Fd, Timer
 *
 * @param createBinding binding creation function
 * @param start         starts I/O operations
 * @param stop          stops I/O operations
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

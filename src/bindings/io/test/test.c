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

#include <tests.h>

#include "test.h"
#include <elektra/io/api.h>
#include <internal/io/private.h>
#include <internal/io/test.h>

static void test_timing (void)
{
	struct timespec start;
	exit_if_fail (elektraIoTestSuiteUtilGetCurrentTime (&start) != 0, "could not measure time; aborting test-suite");
}

static void test_basics (ElektraIoInterface * wrapper)
{
	printf ("test basics\n");

	exit_if_fail (wrapper != NULL, "wrapper is null; aborting test-suite");

	succeed_if (wrapper->addFd != NULL, "addFd is null");
	succeed_if (wrapper->updateFd != NULL, "updateFd is null");
	succeed_if (wrapper->removeFd != NULL, "removeFd is null");

	succeed_if (wrapper->addTimer != NULL, "addTimer is null");
	succeed_if (wrapper->updateTimer != NULL, "updateTimer is null");
	succeed_if (wrapper->removeTimer != NULL, "removeTimer is null");

	succeed_if (wrapper->addIdle != NULL, "addIdle is null");
	succeed_if (wrapper->updateIdle != NULL, "updateIdle is null");
	succeed_if (wrapper->removeIdle != NULL, "removeIdle is null");

	succeed_if (wrapper->cleanup != NULL, "cleanup is null");

	succeed_if (wrapper->cleanup (wrapper), "cleanup did not succeed");
}

/**
 * Test all functions and requirements of the I/O binding returned by createBinding.
 * Requires the following operations: Idle, Timer, Fd
 *
 * @param createBinding binding creation function
 * @param start         starts I/O operations
 * @param stop          stops I/O operations
 */
void elektraIoTestSuite (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop)
{
	printf ("BINDING TEST-SUITE\n");
	printf ("==================\n\n");

	test_timing ();

	test_basics (createBinding ());

	elektraIoTestSuiteIdle (createBinding, start, stop);

	elektraIoTestSuiteTimer (createBinding, start, stop);

	elektraIoTestSuiteFd (createBinding, start, stop);

	elektraIoTestSuiteMix (createBinding, start, stop);
}

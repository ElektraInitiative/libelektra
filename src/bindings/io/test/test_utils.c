/**
 * @file
 *
 * @brief Tests for haskelltemplate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <tests.h>

#include <kdbhelper.h>
#include <kdbio.h>

/**
 * Get current time.
 * Use together with elektraIoTestSuite_getTimeDifference to measure time differences
 * @return timespec
 */
unsigned int elektraIoTestSuiteUtilGetCurrentTime (struct timespec * ts)
{
	if (clock_gettime (CLOCK_MONOTONIC, ts) == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/**
 * Get difference between measured times
 * @param  start start measurement
 * @param  stop  end measurement
 * @return       difference in miliseconds
 */
long elektraIoTestSuiteUtilGetTimeDifference (struct timespec start, struct timespec stop)
{
	struct timespec diff;
	diff.tv_sec = stop.tv_sec - start.tv_sec;
	diff.tv_nsec = stop.tv_nsec - start.tv_nsec;

	return (long)(diff.tv_sec * 1000 + round (diff.tv_nsec / 1.0e6));
}

/**
 * Create timer operation.
 * Free after usage.
 *
 * @return  empty timer operation structure
 */
ElektraIoTimerOperation * elektraIoTestSuiteUtilNewTimerOperation (void)
{
	ElektraIoTimerOperation * timerOp = elektraMalloc (sizeof (*timerOp));
	if (!timerOp)
	{
		exit_if_fail (0, "malloc failed");
		return NULL;
	}
	memset (timerOp, 0, sizeof (*timerOp));

	return timerOp;
}

/**
 * Create idle operation.
 * Free after usage.
 *
 * @return  empty idle operation structure
 */
ElektraIoIdleOperation * elektraIoTestSuiteUtilNewIdleOperation (void)
{
	ElektraIoIdleOperation * idleOp = elektraMalloc (sizeof (*idleOp));
	if (!idleOp)
	{
		exit_if_fail (0, "malloc failed");
		return NULL;
	}
	memset (idleOp, 0, sizeof (*idleOp));

	return idleOp;
}

/**
 * Create fd operation.
 * Free after usage.
 *
 * @return  empty fd operation structure
 */
ElektraIoFdOperation * elektraIoTestSuiteUtilNewFdOperation (void)
{
	ElektraIoFdOperation * fdOp = elektraMalloc (sizeof (*fdOp));
	if (!fdOp)
	{
		exit_if_fail (0, "malloc failed");
		return NULL;
	}
	memset (fdOp, 0, sizeof (*fdOp));

	return fdOp;
}

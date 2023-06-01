/**
 * @file
 *
 * @brief Tests for I/O bindings
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

#include <elektra/io/api.h>
#include <internal/io/test.h>
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
 * @return       difference in milliseconds
 */
long elektraIoTestSuiteUtilGetTimeDifference (struct timespec start, struct timespec stop)
{
	struct timespec diff;
	diff.tv_sec = stop.tv_sec - start.tv_sec;
	diff.tv_nsec = stop.tv_nsec - start.tv_nsec;

	return (long) (diff.tv_sec * 1000 + round (diff.tv_nsec / 1.0e6));
}

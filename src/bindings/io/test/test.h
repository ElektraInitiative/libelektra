/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IOWRAPPER_TESTSUITE_H_
#define KDB_IOWRAPPER_TESTSUITE_H_

#include <elektra/kdbio.h>
#include <kdbiotest.h>
#include <time.h>

unsigned int elektraIoTestSuiteUtilGetCurrentTime (struct timespec * ts);

long elektraIoTestSuiteUtilGetTimeDifference (struct timespec start, struct timespec stop);

void elektraIoTestSuiteIdle (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop);

void elektraIoTestSuiteTimer (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop);

void elektraIoTestSuiteFd (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop);

void elektraIoTestSuiteMix (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop);

#endif

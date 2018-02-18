/**
 * @file
 * @ingroup kdbio
 *
 * @brief Elektra-I/O functions and declarations for the I/O binding test suite
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef KDB_IO_TEST_H_
#define KDB_IO_TEST_H_

#include "kdbio.h"

/**
 * Create and initialize I/O binding.
 *
 * Used by elektraIoTestSuite between tests to get a fresh binding instance.
 *
 * @return initialized I/O binding
 */
typedef ElektraIoInterface * (*ElektraIoTestSuiteCreateBinding) (void);

/**
 * Start I/O processing (for example event loop).
 *
 * Used by elektraIoTestSuite.
 *
 * Should not return until processing is stopped (e.g. by calling
 * ::ElektraIoTestSuiteStop)
 */
typedef void (*ElektraIoTestSuiteStart) (void);

/**
 * Stop I/O processing (for example event loop).
 *
 * Used by elektraIoTestSuite
 */
typedef void (*ElektraIoTestSuiteStop) (void);

/**
 * Test-Suite for I/O Bindings.
 *
 * @param createBinding Create and initialize a new I/O binding instance
 * @param start         Pointer to the start function
 * @param stop          Pointer to the stop function
 */
void elektraIoTestSuite (ElektraIoTestSuiteCreateBinding createBinding, ElektraIoTestSuiteStart start, ElektraIoTestSuiteStop stop);

#endif

/**
 * @file
 *
 * @brief Tests for I/O doc binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <elektra/kdbio.h>
#include <kdbiotest.h>
#include <uv.h>

#include "kdbio_doc.h"

#include <tests.h>

/**
 * @see kdbiotest.h ElektraIoTestSuiteCreateBinding
 */
//! [kdbio testsuite create]
static ElektraIoInterface * createBinding (void)
{
	return elektraIoDocNew ("foo");
}
//! [kdbio testsuite create]

/**
 * @see kdbiotest.h ElektraIoTestSuiteStart
 */
static void startLoop (void)
{
	// someIoLibStartLoop();
}

/**
 * @see kdbiotest.h ElektraIoTestSuiteStop
 */
static void stopLoop (void)
{
	// someIoLibStopLoop();
}

//! [kdbio testsuite main]
int main (int argc, char ** argv)
{
	init (argc, argv);

	elektraIoTestSuite (createBinding, startLoop, stopLoop);

	print_result ("iowrapper_doc");

	return nbError;
}
//! [kdbio testsuite main]

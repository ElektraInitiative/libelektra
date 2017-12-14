/**
 * @file
 *
 * @brief Tests for IO doc binding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <kdbio.h>
#include <uv.h>

#include "io_doc.h"

#include <tests.h>

static ElektraIoInterface * createBinding (void)
{
	return elektraIoDocNew ("foo");
}

static void startLoop (void)
{
	// someIoLibStartLoop();
}

static void stopLoop (void)
{
	// someIoLibStopLoop();
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	elektraIoTestSuite (&createBinding, &startLoop, &stopLoop);

	print_result ("iowrapper_doc");

	return nbError;
}

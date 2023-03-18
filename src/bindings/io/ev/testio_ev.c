/**
 * @file
 *
 * @brief Tests for I/O UV binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <elektra/io/api.h>
#include <internal/io/test.h>
#include <tests.h>

#include <ev.h>

#include <elektra/io/ev.h>

static ElektraIoInterface * createBinding (void)
{
	return elektraIoEvNew (EV_DEFAULT);
}

static void startLoop (void)
{
	ev_run (EV_DEFAULT, 0);
}

static void stopLoop (void)
{
	ev_break (EV_DEFAULT, EVBREAK_ONE);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	elektraIoTestSuite (createBinding, startLoop, stopLoop);

	print_result ("iowrapper_ev");

	ev_loop_destroy (EV_DEFAULT);

	return nbError;
}

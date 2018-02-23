/**
 * @file
 *
 * @brief Implementation of notification functions as defined in kdbnotification.h
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdb.h>
#include <kdbio.h>    // I/O binding functions (elektraIo*)
#include <kdbio_uv.h> // I/O binding constructor for uv (elektraIoUvNew)
#include <kdbnotification.h>

#include <uv.h> // uv functions

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <stdlib.h> // for exit()

static volatile int keepRunning = 0;
static volatile int doUpdateKey = 0;

// from https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
#define ANSI_COLOR_RESET "\x1b[0m"
#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_BLUE "\x1b[34m"

void setTerminalColor (Key * color)
{
	const char * value = keyString (color);
	printf ("Callback called. Changing color to %s\n", value);

	if (strcmp (value, "red") == 0)
	{
		printf (ANSI_COLOR_RED);
	}
	else if (strcmp (value, "green") == 0)
	{
		printf (ANSI_COLOR_GREEN);
	}
	else if (strcmp (value, "blue") == 0)
	{
		printf (ANSI_COLOR_BLUE);
	}
	else
	{
		printf ("Specified color (%s) did not match \"red\", \"green\" or \"blue\". Using default color.\n", value);
		printf (ANSI_COLOR_RESET);
	}
}

void resetTerminalColor (void)
{
	printf (ANSI_COLOR_RESET "\n");
}

void printKeyValue (KeySet * ks, Key * search, char * messageNotSet)
{
	Key * found = ksLookup (ks, search, 0);
	printf ("Key \"%s\"", keyName (search));
	if (!found)
	{
		printf (" not set. %s", messageNotSet);
	}
	else
	{
		printf (" has value \"%s\"", keyString (found));
	}
	printf ("\n");
}

void onSIGNAL (int signal)
{
	if (signal == SIGINT)
	{
		keepRunning = 1;
	}
	if (signal == SIGQUIT)
	{
		doUpdateKey = 1;
	}
}

int main (void)
{
	// Cleanup on SIGINT
	signal (SIGINT, onSIGNAL);
	signal (SIGQUIT, onSIGNAL);

	KeySet * config = ksNew (20, KS_END);

	Key * key = keyNew ("/sw/tests/example_notification/#0/current", KEY_END);
	KDB * kdb = kdbOpen (key);
	if (kdb == NULL)
	{
		printf ("could not open KDB. aborting\n");
		return -1;
	}

	uv_loop_t * loop = uv_default_loop ();
	ElektraIoInterface * binding = elektraIoUvNew (loop);

	int result = elektraNotificationOpen (kdb);
	if (!result)
	{
		printf ("could init notification. aborting\n");
		return -1;
	}


	elektraIoSetBinding (kdb, binding);

	int value = 0;
	Key * intKeyToWatch = keyNew ("/sw/tests/example_notification/#0/current/value", KEY_END);
	result = elektraNotificationRegisterInt (kdb, intKeyToWatch, &value);
	if (!result)
	{
		printf ("could not register variable. aborting\n");
		return -1;
	}

	Key * callbackKeyToWatch = keyNew ("/sw/tests/example_notification/#0/current/color", KEY_END);
	result = elektraNotificationRegisterCallback (kdb, callbackKeyToWatch, &setTerminalColor);
	if (!result)
	{
		printf ("could not register callback. aborting!");
		return -1;
	}

	printf ("Send SIGQUIT (Crtl+\\) to update a key; use SIGINT (Ctl+C) to exit.\n\n");
	// uv_run (loop, UV_RUN_DEFAULT);

	doUpdateKey = 1;

	// repeatedly call kdbGet and print variables
	while (!keepRunning)
	{
		// After this kdbGet the integer variable is updated and the callback was called.
		// TODO remove polling or make it optional when "transport plugins" are available
		kdbGet (kdb, config, key);

		if (doUpdateKey)
		{
			Key * test = keyNew ("system/sw/tests/example_notification/#0/current/asdas", KEY_VALUE, "fooobara", KEY_END);
			Key * parentKey = keyNew ("system/sw/tests/example_notification/#0/current", KEY_END);
			ksAppendKey (config, test);
			kdbSet (kdb, config, parentKey);
			doUpdateKey = 0;
		}

		// Print values
		printf ("My integer value is %d\n", value);
		printKeyValue (config, intKeyToWatch, "Try setting it to any integer value!");
		printKeyValue (config, callbackKeyToWatch, "Try setting it to \"red\", \"green\" or \"blue\"!");
		printf ("\n");

		sleep (2);
	}

	// Cleanup
	resetTerminalColor ();
	elektraNotificationClose (kdb);
	kdbClose (kdb, key);
	ksDel (config);
	keyDel (intKeyToWatch);
	keyDel (callbackKeyToWatch);
	keyDel (key);
	printf ("cleanup done!\n");
}

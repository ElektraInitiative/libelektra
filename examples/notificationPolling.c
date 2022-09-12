/**
 * @file
 *
 * @brief Example for notification library which repeatedly reads some keys and
 * reacts to them; see "notificationAsync.c" for an example without polling
 *
 * Relevant keys for this example:
 *   - /sw/example/notification/#0/current/value: Set to any integer value
 *   - /sw/example/notification/#0/current/color: Set the text color. Possible
 *     values are "red", "green" and "blue".
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdb.h>
#include <kdbhelper.h> // ELEKTRA_UNUSED
#include <kdbnotification.h>

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

static volatile int keepRunning = 0;

// from https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
#define ANSI_COLOR_RESET "\x1b[0m"
#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_BLUE "\x1b[34m"

static void setTerminalColor (ElektraKey * color, void * context ELEKTRA_UNUSED)
{
	const char * value = elektraKeyString (color);
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

static void resetTerminalColor (void)
{
	printf (ANSI_COLOR_RESET "\n");
}

static void printKeyValue (ElektraKeyset * ks, ElektraKey * search, char * messageNotSet)
{
	ElektraKey * found = elektraKeysetLookup (ks, search, 0);
	printf ("Key \"%s\"", elektraKeyName (search));
	if (!found)
	{
		printf (" not set. %s", messageNotSet);
	}
	else
	{
		printf (" has value \"%s\"", elektraKeyString (found));
	}
	printf ("\n");
}

void onSIGINT (int signal)
{
	if (signal == SIGINT)
	{
		keepRunning = 1;
	}
}

int main (void)
{
	// Cleanup on SIGINT
	signal (SIGINT, onSIGINT);

	ElektraKeyset * config = elektraKeysetNew (20, ELEKTRA_KS_END);

	ElektraKeyset * contract = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraNotificationContract (contract);

	ElektraKey * key = elektraKeyNew ("/sw/example/notification/#0/current", ELEKTRA_KEY_END);
	ElektraKdb * kdb = elektraKdbOpen (contract, key);
	if (kdb == NULL)
	{
		printf ("could not open KDB, aborting\n");
		return -1;
	}

	int value = 0;
	ElektraKey * intKeyToWatch = elektraKeyNew ("/sw/example/notification/#0/current/value", ELEKTRA_KEY_END);
	int result = elektraNotificationRegisterInt (kdb, intKeyToWatch, &value);
	if (!result)
	{
		printf ("could not register variable, aborting\n");
		return -1;
	}

	ElektraKey * callbackKeyToWatch = elektraKeyNew ("/sw/example/notification/#0/current/color", ELEKTRA_KEY_END);
	result = elektraNotificationRegisterCallback (kdb, callbackKeyToWatch, &setTerminalColor, NULL);
	if (!result)
	{
		printf ("could not register callback, aborting!");
		return -1;
	}

	printf ("Press Ctl+C to exit.\n\n");

	// repeatedly call kdbGet and print variables
	while (!keepRunning)
	{
		// After this kdbGet the integer variable is updated and the callback was called.
		// see "notificationAsync" for an example without polling
		elektraKdbGet (kdb, config, key);

		// Print values
		printf ("My integer value is %d\n", value);
		printKeyValue (config, intKeyToWatch, "Try setting it to any integer value!");
		printKeyValue (config, callbackKeyToWatch, "Try setting it to \"red\", \"green\" or \"blue\"!");
		printf ("\n");

		sleep (2);
	}

	// Cleanup
	resetTerminalColor ();
	elektraKdbClose (kdb, key);
	elektraKeysetDel (config);
	elektraKeyDel (intKeyToWatch);
	elektraKeyDel (callbackKeyToWatch);
	elektraKeyDel (key);
	printf ("cleanup done!\n");
}

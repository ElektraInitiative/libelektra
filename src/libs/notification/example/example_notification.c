/**
 * @file
 *
 * @brief Implementation of notification functions as defined in kdbnotification.h
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbassert.h>
#include <kdbhelper.h>
#include <kdbnotification.h>

#include <signal.h>
#include <stdio.h>
#include <string.h>

// from https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
#define ANSI_COLOR_RESET "\x1b[0m"
#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_BLUE "\x1b[34m"

void setTerminalColor (Key * color)
{
	const char * value = keyString (color);

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

void debugKeySet (KeySet * ks)
{
	Key * cur;
	ksRewind (ks);
	while ((cur = ksNext (ks)) != NULL)
	{
		printf ("debugKeySet %s = %s\n", keyName (cur), keyString (cur));
	}
	return;
}

int main (int argc, char ** argv)
{
	KeySet * config = ksNew (20, KS_END);

	Key * key = keyNew ("/sw/tests/example_notification/#0/current", KEY_END);
	KDB * kdb = kdbOpen (key);
	ELEKTRA_NOT_NULL (kdb);
	if (kdb == NULL)
	{
		printf ("could not open KDB. aborting\n");
		return -1;
	}

	int result = elektraNotificationOpen (kdb, NULL);
	if (!result)
	{
		printf ("could init notification. aborting\n");
		return -1;
	}

	int value = 0;
	Key * valueKey = keyNew ("/sw/tests/example_notification/#0/current/value", KEY_END);
	result = elektraNotificationRegisterInt (kdb, valueKey, &value);
	if (!result)
	{
		printf ("could not register variable. aborting\n");
		return -1;
	}

	Key * colorKey = keyNew ("/sw/tests/example_notification/#0/current/color", KEY_END);
	result = elektraNotificationRegisterCallback (kdb, colorKey, &setTerminalColor);
	if (!result)
	{
		printf ("could not register callback. abortin!");
		return -1;
	}

	kdbGet (kdb, config, key);
	debugKeySet (config);

	printf ("Heyo! My value is %d\n", value);
	resetTerminalColor ();
}

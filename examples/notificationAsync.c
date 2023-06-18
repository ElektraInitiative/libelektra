/**
 * @file
 *
 * @brief Example for notification library which repeatedly reads some keys and
 * reacts to them
 *
 * Requires:
 *   - io_uv binding
 *   - Transport plugins (e.g. kdb global-mount dbus announce=once dbusrecv)
 *
 * Relevant keys for this example:
 *   - /sw/example/notification/#0/current/value: Set to any integer value
 *   - /sw/example/notification/#0/current/color: Set the text color. Possible
 *     values are "red", "green" and "blue".
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/io/api.h> // I/O binding functions (elektraIo*)
#include <elektra/io/uv.h>  // I/O binding constructor for uv (elektraIoUvNew)
#include <elektra/kdb/kdb.h>
#include <elektra/notifications.h> // notification functions
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>
#include <uv.h> // uv functions

#include <signal.h> // signal()
#include <stdio.h>  // printf() & co

uv_async_t wakeup;

#ifdef HAVE_LIBUV0
static void wakeupCallback (uv_async_t * async ELEKTRA_UNUSED, int unknown ELEKTRA_UNUSED)
{
	// nothing to do; callback required for libuv 0.x
}
#endif

// from https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
#define ANSI_COLOR_RESET "\x1b[0m"
#define ANSI_COLOR_RED "\x1b[31m"
#define ANSI_COLOR_GREEN "\x1b[32m"
#define ANSI_COLOR_BLUE "\x1b[34m"

static void setTerminalColor (Key * color, void * context ELEKTRA_UNUSED)
{
	const char * value = keyString (color);
	printf ("Callback called. Changing color to %s\n", value);

	if (elektraStrCmp (value, "red") == 0)
	{
		printf (ANSI_COLOR_RED);
	}
	else if (elektraStrCmp (value, "green") == 0)
	{
		printf (ANSI_COLOR_GREEN);
	}
	else if (elektraStrCmp (value, "blue") == 0)
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

static void onSIGNAL (int signal)
{
	if (signal == SIGINT)
	{
		uv_stop (uv_default_loop ());
		// Without this call the loop would be "sleeping" until the next timer interval
		uv_async_send (&wakeup);
	}
}

static void printVariable (ElektraIoTimerOperation * timerOp)
{
	int value = *(int *) elektraIoTimerGetData (timerOp);
	printf ("\nMy integer value is %d\n", value);
}

int main (void)
{
	// Cleanup on SIGINT
	signal (SIGINT, onSIGNAL);

	KeySet * config = ksNew (20, KS_END);

	uv_loop_t * loop = uv_default_loop ();
	ElektraIoInterface * binding = elektraIoUvNew (loop);

	KeySet * contract = ksNew (0, KS_END);
	elektraIoContract (contract, binding);
	elektraNotificationContract (contract);

	Key * key = keyNew ("/sw/example/notification/#0/current", KEY_END);
	KDB * kdb = elektraKdbOpen (contract, key);
	if (kdb == NULL)
	{
		printf ("could not open KDB, aborting\n");
		return -1;
	}

	int value = 0;
	Key * intKeyToWatch = keyNew ("/sw/example/notification/#0/current/value", KEY_END);
	int result = elektraNotificationRegisterInt (kdb, intKeyToWatch, &value);
	if (!result)
	{
		printf ("could not register variable, aborting\n");
		return -1;
	}

	Key * callbackKeyToWatch = keyNew ("/sw/example/notification/#0/current/color", KEY_END);
	result = elektraNotificationRegisterCallback (kdb, callbackKeyToWatch, &setTerminalColor, NULL);
	if (!result)
	{
		printf ("could not register callback, aborting!");
		return -1;
	}

	// Setup timer that repeatedly prints the variable
	ElektraIoTimerOperation * timer = elektraIoNewTimerOperation (2000, 1, printVariable, &value);
	elektraIoBindingAddTimer (binding, timer);

	printf ("Asynchronous Notification Example Application\n");
	printf ("Please note that notification transport plugins are required see\n"
		" https://www.libelektra.org/tutorials/notifications#notification-configuration!\n");
	printf ("- Set \"%s\" to red, blue or green to change the text color\n", keyName (callbackKeyToWatch));
	printf ("- Set \"%s\" to any integer value\n", keyName (intKeyToWatch));
	printf ("Send SIGINT (Ctl+C) to exit.\n\n");

	// Get configuration
	elektraKdbGet (kdb, config, key);
	printVariable (timer); // "value" was automatically updated

	// This allows us to wake the loop from our signal handler
#ifdef HAVE_LIBUV1
	uv_async_init (loop, &wakeup, NULL);
#else
	uv_async_init (loop, &wakeup, wakeupCallback);
#endif

	uv_run (loop, UV_RUN_DEFAULT);

	// Cleanup
	resetTerminalColor ();
	elektraIoBindingRemoveTimer (timer);
	elektraFree (timer);
	elektraKdbClose (kdb, key);

	elektraIoBindingCleanup (binding);
	uv_run (loop, UV_RUN_NOWAIT);
#ifdef HAVE_LIBUV1
	uv_loop_close (uv_default_loop ());
#else
	uv_loop_delete (uv_default_loop ());
#endif

	ksDel (config);
	keyDel (intKeyToWatch);
	keyDel (callbackKeyToWatch);
	keyDel (key);
	printf ("cleanup done!\n");
}

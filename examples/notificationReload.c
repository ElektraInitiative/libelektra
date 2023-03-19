/**
 * @file
 *
 * @brief Example for notification library which reloads KDB when Elektra's
 * configuration (e.g. mount points or global plugins) has changed.
 * This example also shows how to pass user data using elektraIo*GetData().
 *
 * Requires:
 *   - io_glib binding
 *   - Transport plugins (e.g. kdb global-mount zeromqsend zeromqrecv && kdb run-hub-zeromq)
 *
 * Relevant keys for this example:
 *   - /sw/example/notification/#0/current/value: Set to any integer value
 *   Add additional transport plugins and remove the original pair afterwards or
 *   mount a file which sets the key above to a different value and unmount it again
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/io/api.h>	   // I/O binding functions (elektraIo*)
#include <elektra/io/glib.h>	   // I/O binding constructor for glib (elektraIoGlibNew)
#include <elektra/notifications.h> // notification functions
#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <elektra/kdb/kdb.h>
#include <internal/utility/old_helper.h> // elektraFree

#include <glib-unix.h> // g_unix_signal_add()
#include <glib.h>      // glib functions

#include <signal.h> // signal()
#include <stdio.h>  // printf() & co
#include <stdlib.h> // exit()

#define TWO_SECONDS 2000
#define RELOAD_INTERVAL 100

/**
 * Data container for this example to demo
 * usage of the elektraIo*GetData() functions.
 *
 * Members could also be globals.
 */
typedef struct ExampleUserData
{
	GMainLoop * loop;
	KDB * kdb;
	Key * parentKey;
	KeySet * config;
	ElektraIoInterface * binding;
	Key * intKeyToWatch;
	int valueToPrint;
	ElektraIoTimerOperation * timer;
	ElektraIoTimerOperation * reload;
} ExampleUserData;

static void elektraChangedCallback (Key * changedKey ELEKTRA_UNUSED, void * context);

/**
 * Initializes KDB on first call and performs cleanup before initialization on
 * subsequent calls.
 *
 * @param timerOp unused
 */
static void initKdb (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	ExampleUserData * data = (ExampleUserData *) elektraIoTimerGetData (timerOp);

	int didReload = 0;

	// Stop reload task
	elektraIoTimerSetEnabled (data->reload, 0);
	elektraIoBindingUpdateTimer (data->reload);

	if (data->kdb != NULL)
	{
		// Cleanup notifications and close KDB
		kdbClose (data->kdb, data->parentKey);
		didReload = 1;
	}

	KeySet * contract = ksNew (0, KS_END);
	elektraIoContract (contract, data->binding);
	elektraNotificationContract (contract);

	data->kdb = kdbOpen (contract, data->parentKey);
	if (data->kdb == NULL)
	{
		printf ("could not open KDB, aborting\n");
		exit (1);
	}

	int result = elektraNotificationRegisterInt (data->kdb, data->intKeyToWatch, &data->valueToPrint);
	if (!result)
	{
		printf ("could not register variable, aborting\n");
		exit (1);
	}

	Key * elektraKey = keyNew ("/elektra", KEY_END);
	if (!elektraNotificationRegisterCallbackSameOrBelow (data->kdb, elektraKey, elektraChangedCallback, data))
	{
		printf ("could not register for changes to Elektra's configuration, aborting\n");
		exit (1);
	}
	keyDel (elektraKey);

	// Get configuration
	kdbGet (data->kdb, data->config, data->parentKey);

	if (didReload)
	{
		printf ("KDB reloaded.\n");
	}
}

static gboolean onSIGNAL (gpointer user_data)
{
	ExampleUserData * data = (ExampleUserData *) user_data;
	// Cleanup
	elektraIoBindingRemoveTimer (data->timer);
	elektraFree (data->timer);
	elektraIoBindingRemoveTimer (data->reload);
	elektraFree (data->reload);
	kdbClose (data->kdb, data->parentKey);
	elektraIoBindingCleanup (data->binding);

	g_main_loop_quit (data->loop);
	return FALSE;
}

/**
 * This function is called whenever Elektra's configuration has changed.
 *
 * @param changedKey unused
 * @param context unused
 */
static void elektraChangedCallback (Key * changedKey ELEKTRA_UNUSED, void * context)
{
	printf ("\nElektra's configuration has changed.\n");

	// Enable operation to reload KDB as soon as possible
	ExampleUserData * data = (ExampleUserData *) context;
	elektraIoTimerSetEnabled (data->reload, 1);
	elektraIoBindingUpdateTimer (data->reload);
}

static void printVariable (ElektraIoTimerOperation * timerOp)
{
	// int value = *(int *) elektraIoTimerGetData (timerOp);
	ExampleUserData * data = (ExampleUserData *) elektraIoTimerGetData (timerOp);
	printf ("\nMy integer value is %d\n", data->valueToPrint);
}

int main (void)
{
	ExampleUserData * data = elektraCalloc (sizeof (*data));
	if (data == NULL)
	{
		printf ("elektraCalloc failed");
		return 1;
	}

	// Create glib main loop
	GMainContext * context = NULL; // use default context
	data->loop = g_main_loop_new (context, 0);
	data->binding = elektraIoGlibNew (context);

	// Signal Handling
	g_unix_signal_add (SIGINT, onSIGNAL, data);

	data->config = ksNew (20, KS_END);
	data->parentKey = keyNew ("/sw/example/notification/#0/current", KEY_END);
	data->intKeyToWatch = keyNew ("/sw/example/notification/#0/current/value", KEY_END);

	// Setup timer that repeatedly prints the variable
	data->timer = elektraIoNewTimerOperation (TWO_SECONDS, 1, printVariable, data);
	elektraIoBindingAddTimer (data->binding, data->timer);

	// Setup timer for reloading Elektra's configuration
	data->reload = elektraIoNewTimerOperation (RELOAD_INTERVAL, 0, initKdb, data);
	elektraIoBindingAddTimer (data->binding, data->reload);

	printf ("Reloading Notification Example Application\n");
	printf ("Please note that notification transport plugins are required see\n"
		" https://www.libelektra.org/tutorials/notifications#notification-configuration!\n");
	printf ("- Set \"%s\" to any integer value\n", keyName (data->intKeyToWatch));
	printf ("- Try to add additional transport plugins and remove the original pair afterwards\n");
	printf ("- Mount a file which sets the key above to a different value and unmount it\n");
	printf ("Send SIGINT (Ctl+C) to exit.\n\n");

	// Initialize KDB
	initKdb (data->reload);
	printVariable (data->timer); // "value" was automatically updated

	g_main_loop_run (data->loop);

	g_main_loop_unref (data->loop);

	ksDel (data->config);
	keyDel (data->intKeyToWatch);
	keyDel (data->parentKey);
	elektraFree (data);
	printf ("cleanup done!\n");
}

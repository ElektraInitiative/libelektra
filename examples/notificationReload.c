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

#include <kdb.h>
#include <kdbhelper.h>	     // elektraFree
#include <kdbio.h>	     // I/O binding functions (elektraIo*)
#include <kdbio/glib.h>	     // I/O binding constructor for glib (elektraIoGlibNew)
#include <kdbnotification.h> // notification functions

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
	ElektraKdb * kdb;
	ElektraKey * parentKey;
	ElektraKeyset * config;
	ElektraIoInterface * binding;
	ElektraKey * intKeyToWatch;
	int valueToPrint;
	ElektraIoTimerOperation * timer;
	ElektraIoTimerOperation * reload;
} ExampleUserData;

static void elektraChangedCallback (ElektraKey * changedKey ELEKTRA_UNUSED, void * context);

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
		elektraKdbClose (data->kdb, data->parentKey);
		didReload = 1;
	}

	ElektraKeyset * contract = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraIoContract (contract, data->binding);
	elektraNotificationContract (contract);

	data->kdb = elektraKdbOpen (contract, data->parentKey);
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

	ElektraKey * elektraKey = elektraKeyNew ("/elektra", ELEKTRA_KEY_END);
	if (!elektraNotificationRegisterCallbackSameOrBelow (data->kdb, elektraKey, elektraChangedCallback, data))
	{
		printf ("could not register for changes to Elektra's configuration, aborting\n");
		exit (1);
	}
	elektraKeyDel (elektraKey);

	// Get configuration
	elektraKdbGet (data->kdb, data->config, data->parentKey);

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
	elektraKdbClose (data->kdb, data->parentKey);
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
static void elektraChangedCallback (ElektraKey * changedKey ELEKTRA_UNUSED, void * context)
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

	data->config = elektraKeysetNew (20, ELEKTRA_KS_END);
	data->parentKey = elektraKeyNew ("/sw/example/notification/#0/current", ELEKTRA_KEY_END);
	data->intKeyToWatch = elektraKeyNew ("/sw/example/notification/#0/current/value", ELEKTRA_KEY_END);

	// Setup timer that repeatedly prints the variable
	data->timer = elektraIoNewTimerOperation (TWO_SECONDS, 1, printVariable, data);
	elektraIoBindingAddTimer (data->binding, data->timer);

	// Setup timer for reloading Elektra's configuration
	data->reload = elektraIoNewTimerOperation (RELOAD_INTERVAL, 0, initKdb, data);
	elektraIoBindingAddTimer (data->binding, data->reload);

	printf ("Reloading Notification Example Application\n");
	printf ("Please note that notification transport plugins are required see\n"
		" https://www.libelektra.org/tutorials/notifications#notification-configuration!\n");
	printf ("- Set \"%s\" to any integer value\n", elektraKeyName (data->intKeyToWatch));
	printf ("- Try to add additional transport plugins and remove the original pair afterwards\n");
	printf ("- Mount a file which sets the key above to a different value and unmount it\n");
	printf ("Send SIGINT (Ctl+C) to exit.\n\n");

	// Initialize KDB
	initKdb (data->reload);
	printVariable (data->timer); // "value" was automatically updated

	g_main_loop_run (data->loop);

	g_main_loop_unref (data->loop);

	elektraKeysetDel (data->config);
	elektraKeyDel (data->intKeyToWatch);
	elektraKeyDel (data->parentKey);
	elektraFree (data);
	printf ("cleanup done!\n");
}

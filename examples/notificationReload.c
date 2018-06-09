/**
 * @file
 *
 * @brief Example for notification library which reloads KDB when Elektra's
 * configuration (e.g. mount points or global plugins) has changed.
 *
 * Requires:
 *   - io_glib binding
 *   - Transport plugins (e.g. kdb global-mount zeromqsend zeromqrecv && kdb run-hub-zeromq)
 *
 * Ideas for this example:
 *   - /sw/tests/example_notification/#0/current/value: Set to any integer value
 *   - Try to add additional transport plugins and remove the original pair afterwards
 *   - Mount a file which sets the key above to a different value and unmount it
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdb.h>
#include <kdbhelper.h>       // elektraFree
#include <kdbio.h>	   // I/O binding functions (elektraIo*)
#include <kdbio/glib.h>      // I/O binding constructor for glib (elektraIoGlibNew)
#include <kdbnotification.h> // notification functions

#include <glib-unix.h> // g_unix_signal_add()
#include <glib.h>      // glib functions

#include <signal.h> // signal()
#include <stdio.h>  // printf() & co
#include <stdlib.h> // exit()

GMainLoop * loop;

KDB * kdb;
Key * parentKey;
KeySet * config;
ElektraIoInterface * binding;
Key * intKeyToWatch;
int valueToPrint = 0;

ElektraIoTimerOperation * timer;
ElektraIoTimerOperation * reload;

static void elektraChangedCallback (Key * changedKey ELEKTRA_UNUSED, void * context ELEKTRA_UNUSED);

/**
 * Initializes KDB on first call and performs cleanup before initialization on
 * subsequent calls.
 *
 * @param timerOp unused
 */
static void initKdb (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	int didReload = 0;

	// Stop reload task
	elektraIoTimerSetEnabled (reload, 0);
	elektraIoBindingUpdateTimer (reload);

	if (kdb != NULL)
	{
		// Cleanup notifications and close KDB
		elektraNotificationClose (kdb);
		kdbClose (kdb, parentKey);
		didReload = 1;
	}

	kdb = kdbOpen (parentKey);
	if (kdb == NULL)
	{
		printf ("could not open KDB. aborting\n");
		exit (-1);
	}

	elektraIoSetBinding (kdb, binding);

	int result = elektraNotificationOpen (kdb);
	if (!result)
	{
		printf ("could not init notification. aborting\n");
		exit (-1);
	}

	result = elektraNotificationRegisterInt (kdb, intKeyToWatch, &valueToPrint);
	if (!result)
	{
		printf ("could not register variable. aborting\n");
		exit (-1);
	}

	Key * elektraKey = keyNew ("system/elektra", KEY_END);
	if (!elektraNotificationRegisterCallbackSameOrBelow (kdb, elektraKey, elektraChangedCallback, NULL))
	{
		printf ("could not register for changes to Elektra's configuration. aborting\n");
		exit (-1);
	}
	keyDel (elektraKey);

	// Get configuration
	kdbGet (kdb, config, parentKey);

	if (didReload)
	{
		printf ("KDB reloaded.\n");
	}
}

static gboolean onSIGNAL (gpointer user_data ELEKTRA_UNUSED)
{
	// Cleanup
	elektraIoBindingRemoveTimer (timer);
	elektraFree (timer);
	elektraIoBindingRemoveTimer (reload);
	elektraFree (reload);
	elektraNotificationClose (kdb);
	kdbClose (kdb, parentKey);
	elektraIoBindingCleanup (binding);

	g_main_loop_quit (loop);
	return FALSE;
}

/**
 * This function is called whenever Elektra's configuration has changed.
 * Since cannot call elektraNotificationClose() here we start a timer operation
 * which allows us to reload KDB in the next main loop iteration.
 *
 * @param changedKey unused
 * @param context unused
 */
static void elektraChangedCallback (Key * changedKey ELEKTRA_UNUSED, void * context ELEKTRA_UNUSED)
{
	printf ("\nElektra's configuration has changed.\n");

	// Enable operation to reload KDB as soon as possible
	elektraIoTimerSetEnabled (reload, 1);
	elektraIoBindingUpdateTimer (reload);
}

static void printVariable (ElektraIoTimerOperation * timerOp)
{
	int value = *(int *) elektraIoTimerGetData (timerOp);
	printf ("\nMy integer value is %d\n", value);
}

int main (void)
{
	// Create glib main loop
	GMainContext * context = NULL; // use default context
	loop = g_main_loop_new (context, 0);
	binding = elektraIoGlibNew (context);

	// Signal Handling
	g_unix_signal_add (SIGINT, onSIGNAL, NULL);

	config = ksNew (20, KS_END);
	parentKey = keyNew ("/sw/tests/example_notification/#0/current", KEY_END);
	intKeyToWatch = keyNew ("/sw/tests/example_notification/#0/current/value", KEY_END);

	// Setup timer that repeatedly prints the variable
	timer = elektraIoNewTimerOperation (2000, 1, printVariable, &valueToPrint);
	elektraIoBindingAddTimer (binding, timer);

	// Setup timer for reloading Elektra's configuration
	reload = elektraIoNewTimerOperation (100, 0, initKdb, NULL);
	elektraIoBindingAddTimer (binding, reload);

	printf ("Reloading Notification Example Application\n");
	printf ("- Set \"%s\" to any integer value\n", keyName (intKeyToWatch));
	printf ("- Try to add additional transport plugins and remove the original pair afterwards\n");
	printf ("- Mount a file which sets the key above to a different value and unmount it\n");
	printf ("Send SIGINT (Ctl+C) to exit.\n\n");

	// Initialize KDB
	initKdb (reload);
	printVariable (timer); // "value" was automatically updated

	g_main_loop_run (loop);

	g_main_loop_unref (loop);

	ksDel (config);
	keyDel (intKeyToWatch);
	keyDel (parentKey);
	printf ("cleanup done!\n");
}

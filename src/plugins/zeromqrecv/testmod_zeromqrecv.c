/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./zeromqrecv.h"

#include <stdio.h>  // printf() & co
#include <time.h>   // time()
#include <unistd.h> // usleep()

#include <elektra/io/plugin.h> // ElektraIoPluginSetBinding
#include <elektra/io/uv.h>     // elektraIoUvNew()

#include <tests.h>
#include <tests_plugin.h>

#include <uv.h>

#include <internal/macros/attributes.h>
#include <internal/utility/string.h>

/** zmq context for tests */
void * context;

/** time in microseconds to wait until zmq connections are established and sending & receiving works */
#define TIME_SETTLE_US (1000 * 1000)

/** time (100ms in microseconds) before a new socket is created. leaves the system some after binding a socket again */
#define TIME_HOLDOFF (100 * 1000)

/** timeout for tests in seconds */
#define TEST_TIMEOUT 10

Key * test_callbackKey;
uv_loop_t * test_callbackLoop;
int test_incompleteMessageTimeout;

/**
 * @internal
 * Create publisher socket for tests.
 *
 * @return  new socket
 */
static void * createTestSocket (void)
{
	// leave the system some time before binding again
	usleep (TIME_HOLDOFF);

	void * pubSocket = zmq_socket (context, ZMQ_PUB);
	int result = zmq_bind (pubSocket, "tcp://127.0.0.1:6001");
	if (result != 0)
	{
		yield_error ("zmq_bind failed");
		printf ("zmq error was: %s\n", zmq_strerror (zmq_errno ()));
		exit (-1);
	}

	return pubSocket;
}

/**
 * @internal
 * Send a notification over a socket.
 *
 * @param socket     ZeroMq socket
 * @param changeType change type
 * @param keyName    key name
 */
static void sendTestNotification (void * socket, char * changeType, char * keyName)
{
	succeed_if (zmq_send (socket, changeType, elektraStrLen (changeType), ZMQ_SNDMORE) != -1, "failed to send change type");
	succeed_if (zmq_send (socket, keyName, elektraStrLen (keyName), 0) != -1, "failed to send key name");
}

/**
 * @internal
 * Called by plugin when a notification was received.
 * The key is saved to be evaluated by the current test and the event loop is
 * stopped.
 *
 * @see ElektraNotificationCallback (kdbnotificationinternal.h)
 *
 * @param key     changed key
 * @param context notification callback context
 */
static void test_notificationCallback (Key * key, ElektraNotificationCallbackContext * callbackContext ELEKTRA_UNUSED)
{
	test_callbackKey = key;
	uv_stop (test_callbackLoop);
}

/**
 * Timeout for tests.
 *
 * Creates a failure and stops the event loop
 *
 * @param timerOp timer operation
 */
static void test_timerCallback (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	yield_error ("timeout exceeded; test failed");
	uv_stop (test_callbackLoop);
}

/**
 * Timeout for incomplete message test.
 *
 * Creates stops the event loop
 *
 * @param timerOp timer operation
 */
static void test_timerCallbackIncomplete (ElektraIoTimerOperation * timerOp ELEKTRA_UNUSED)
{
	test_incompleteMessageTimeout = 1;
	uv_stop (test_callbackLoop);
}

static void test_commit (uv_loop_t * loop, ElektraIoInterface * binding)
{
	printf ("test commit notification\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("zeromqrecv");

	void * pubSocket = createTestSocket ();

	ksDel (plugin->global);
	plugin->global =
		ksNew (5, keyNew ("system:/elektra/io/binding", KEY_BINARY, KEY_SIZE, sizeof (binding), KEY_VALUE, &binding, KEY_END),
		       keyNew ("system:/elektra/notification/callback", KEY_FUNC, test_notificationCallback, KEY_END), KS_END);
	// call open again after correctly setting up global keyset
	plugin->kdbOpen (plugin, NULL);

	usleep (TIME_SETTLE_US);

	char * changeType = "Commit";
	char * expectedKeyName = "system:/foo/bar";
	sendTestNotification (pubSocket, changeType, expectedKeyName);

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TEST_TIMEOUT * 1000, 1, test_timerCallback, NULL);
	elektraIoBindingAddTimer (binding, timerOp);

	test_callbackKey = NULL;
	test_callbackLoop = loop;
	uv_run (loop, UV_RUN_DEFAULT);

	succeed_if_same_string (expectedKeyName, keyName (test_callbackKey));

	zmq_close (pubSocket);

	elektraIoBindingRemoveTimer (timerOp);
	elektraFree (timerOp);
	keyDel (test_callbackKey);
	ksDel (plugin->global);
	PLUGIN_CLOSE ();
}

static void test_incompleteMessage (uv_loop_t * loop, ElektraIoInterface * binding)
{
	printf ("test incomplete message\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("zeromqrecv");

	void * pubSocket = createTestSocket ();

	ksDel (plugin->global);
	plugin->global =
		ksNew (5, keyNew ("system:/elektra/io/binding", KEY_BINARY, KEY_SIZE, sizeof (binding), KEY_VALUE, &binding, KEY_END),
		       keyNew ("system:/elektra/notification/callback", KEY_FUNC, test_notificationCallback, KEY_END), KS_END);
	// call open again after correctly setting up global keyset
	plugin->kdbOpen (plugin, NULL);

	usleep (TIME_SETTLE_US);

	char * changeType = "KeyChanged";
	char * expectedKeyName = "system:/foo/bar";
	// send message parts as standalone messages
	succeed_if (zmq_send (pubSocket, changeType, elektraStrLen (changeType), 0 /* no ZMQ_SNDMORE here */) != -1,
		    "failed to send change type");
	succeed_if (zmq_send (pubSocket, expectedKeyName, elektraStrLen (expectedKeyName), 0) != -1, "failed to send change type");

	ElektraIoTimerOperation * timerOp = elektraIoNewTimerOperation (TEST_TIMEOUT * 1000, 1, test_timerCallbackIncomplete, NULL);
	elektraIoBindingAddTimer (binding, timerOp);

	test_callbackKey = NULL;
	test_callbackLoop = loop;
	test_incompleteMessageTimeout = 0;
	uv_run (loop, UV_RUN_DEFAULT);

	succeed_if (test_incompleteMessageTimeout, "test did not timeout");
	succeed_if (test_callbackKey == NULL, "should not receive key");

	zmq_close (pubSocket);

	elektraIoBindingRemoveTimer (timerOp);
	elektraFree (timerOp);
	ksDel (plugin->global);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("ZEROMQRECV TESTS\n");
	printf ("================\n\n");

	init (argc, argv);

	int major, minor, patch;
	zmq_version (&major, &minor, &patch);
	printf ("zeromq version is %d.%d.%d\n", major, minor, patch);

	context = zmq_ctx_new ();

	uv_loop_t * loop = uv_default_loop ();
	ElektraIoInterface * binding = elektraIoUvNew (loop);

	test_commit (loop, binding);
	test_incompleteMessage (loop, binding);

	print_result ("testmod_zeromqrecv");

	elektraIoBindingCleanup (binding);

	zmq_ctx_destroy (context);

	while (uv_run (loop, UV_RUN_NOWAIT) != 0)
		;
#ifdef HAVE_LIBUV1
	uv_loop_close (loop);
#elif HAVE_LIBUV0
	uv_loop_delete (loop);
#endif
	return nbError;
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "zeromqsend.h"

#include <stdio.h>  // printf() & co
#include <time.h>   // time()
#include <unistd.h> // usleep()

#include <kdberrors.h>	 // TIMEOUT ERROR
#include <kdbioplugin.h> // ElektraIoPluginSetBinding

#include <tests.h>
#include <tests_plugin.h>

#include <pthread.h>

/** change type received by readNotificationFromTestSocket() */
char * receivedChangeType;

/** key name received by readNotificationFromTestSocket() */
char * receivedKeyName;

/** variable indicating that a timeout occurred while receiving */
int receiveTimeout;

/** zmq context for tests */
void * context;

/** time in microseconds before a new socket is created. leaves the system some after binding a socket again */
#define TIME_HOLDOFF (1000 * 1000)

/** timeout for tests in seconds */
#define TEST_TIMEOUT 20

/** endpoint for tests */
#define TEST_ENDPOINT "tcp://127.0.0.1:6002"

/** extended timeouts for tests */
#define TESTCONFIG_CONNECT_TIMEOUT "5000"
#define TESTCONFIG_SUBSCRIBE_TIMEOUT "5000"

/**
 * Create subscriber socket for tests.
 * @internal
 *
 * @param  subscribeFilter filter for subscriptions
 * @return                 new socket
 */
static void * createTestSocket (char * subscribeFilter)
{
	// leave the system some time before binding again
	usleep (TIME_HOLDOFF);

	void * subSocket = zmq_socket (context, ZMQ_SUB);
	int result = zmq_bind (subSocket, TEST_ENDPOINT);
	if (result != 0)
	{
		yield_error ("zmq_bind failed");
		printf ("zmq error was: %s\n", zmq_strerror (zmq_errno ()));
		exit (-1);
	}
	if (subscribeFilter != NULL)
	{
		exit_if_fail (zmq_setsockopt (subSocket, ZMQ_SUBSCRIBE, subscribeFilter, elektraStrLen (subscribeFilter)) == 0,
			      "subscribe failed"); // subscribe to all messages
	}
	return subSocket;
}

/**
 * Main function for notification reader thread.
 *
 * Sets global variables receivedKeyName and receivedChangeType.
 *
 * @internal
 *
 * @param  subSocket socket to read messages from
 * @return           always NULL
 */
static void * notificationReaderThreadMain (void * filter)
{
	void * subSocket = createTestSocket ((char *) filter);

	time_t start = time (NULL);

	zmq_msg_t message;
	zmq_msg_init (&message);
	int more;
	size_t moreSize = sizeof (more);
	int rc;
	int partCounter = 0;
	int maxParts = 2; // change type and key name
	int lastErrno;
	do
	{
		usleep (100 * 1000); // wait 100 ms

		lastErrno = 0;
		int result = zmq_msg_recv (&message, subSocket, ZMQ_DONTWAIT);

		// check for timeout
		if (time (NULL) - start > TEST_TIMEOUT)
		{
			receiveTimeout = 1;
			receivedChangeType = NULL;
			receivedKeyName = NULL;
			zmq_msg_close (&message);
			zmq_close (subSocket);
			return NULL;
		}

		// check for errors
		if (result == -1)
		{
			lastErrno = zmq_errno ();
			if (lastErrno != EAGAIN)
			{
				yield_error ("zmq_msg_recv failed");
				printf ("zmq_msg_recv failed: %s\n", zmq_strerror (lastErrno));
				zmq_msg_close (&message);
				zmq_close (subSocket);
				return NULL;
			}
		}
		else
		{
			rc = zmq_getsockopt (subSocket, ZMQ_RCVMORE, &more, &moreSize);
			if (rc < 0)
			{
				yield_error ("zmq_getsockopt failed");
				printf ("zmq_getsockopt failed: %s\n", zmq_strerror (zmq_errno ()));
				zmq_msg_close (&message);
				zmq_close (subSocket);
				return NULL;
			}

			int length = zmq_msg_size (&message);
			char * buffer = elektraMemDup (zmq_msg_data (&message), length + 1);
			buffer[length] = '\0';

			switch (partCounter)
			{
			case 0:
				receivedChangeType = buffer;
				break;
			case 1:
				receivedKeyName = buffer;
				break;
			default:
				yield_error ("test inconsistency");
			}

			partCounter++;
		}
	} while (lastErrno == EAGAIN || (more && partCounter < maxParts));

	zmq_msg_close (&message);
	zmq_close (subSocket);

	return NULL;
}

/**
 * Create and start thread for reading notifications.
 * @internal
 *
 * @param  filter subscription filter
 * @return        new thread
 */
static pthread_t * startNotificationReaderThread (char * filter)
{
	pthread_t * thread = elektraMalloc (sizeof *thread);
	pthread_create (thread, NULL, notificationReaderThreadMain, filter);
	return thread;
}

static void test_commit (void)
{
	printf ("test commit notification\n");

	Key * parentKey = keyNew ("system:/tests/foo", KEY_END);
	Key * toAdd = keyNew ("system:/tests/foo/bar", KEY_END);
	KeySet * ks = ksNew (0, KS_END);

	KeySet * conf = ksNew (3, keyNew ("/endpoint", KEY_VALUE, TEST_ENDPOINT, KEY_END),
			       keyNew ("/connectTimeout", KEY_VALUE, TESTCONFIG_CONNECT_TIMEOUT, KEY_END),
			       keyNew ("/subscribeTimeout", KEY_VALUE, TESTCONFIG_SUBSCRIBE_TIMEOUT, KEY_END), KS_END);
	PLUGIN_OPEN ("zeromqsend");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	receiveTimeout = 0;
	receivedKeyName = NULL;
	receivedChangeType = NULL;

	pthread_t * thread = startNotificationReaderThread ("Commit");
	plugin->kdbCommit (plugin, ks, parentKey);
	pthread_join (*thread, NULL);

	succeed_if (receiveTimeout == 0, "receiving did time out");
	succeed_if (!keyGetMeta (parentKey, "warnings"), "warning meta key was set");
	succeed_if_same_string ("Commit", receivedChangeType);
	succeed_if_same_string (keyName (parentKey), receivedKeyName);

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
	elektraFree (receivedKeyName);
	elektraFree (receivedChangeType);
	elektraFree (thread);
}

static void test_timeoutConnect (void)
{
	printf ("test connect timeout\n");

	Key * parentKey = keyNew ("system:/tests/foo", KEY_END);
	Key * toAdd = keyNew ("system:/tests/foo/bar", KEY_END);
	KeySet * ks = ksNew (0, KS_END);

	KeySet * conf = ksNew (3, keyNew ("/endpoint", KEY_VALUE, TEST_ENDPOINT, KEY_END),
			       keyNew ("/connectTimeout", KEY_VALUE, TESTCONFIG_CONNECT_TIMEOUT, KEY_END),
			       keyNew ("/subscribeTimeout", KEY_VALUE, TESTCONFIG_SUBSCRIBE_TIMEOUT, KEY_END), KS_END);
	PLUGIN_OPEN ("zeromqsend");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	plugin->kdbCommit (plugin, ks, parentKey);

	char * expectedWarningNumber = elektraFormat ("%s", ELEKTRA_ERROR_INSTALLATION);
	succeed_if (keyGetMeta (parentKey, "warnings"), "warning meta key was not set");
	succeed_if_same_string (expectedWarningNumber, keyValue (keyGetMeta (parentKey, "warnings/#0/number")));

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
	elektraFree (expectedWarningNumber);
}

static void test_timeoutSubscribe (void)
{
	printf ("test subscribe message timeout\n");

	Key * parentKey = keyNew ("system:/tests/foo", KEY_END);
	Key * toAdd = keyNew ("system:/tests/foo/bar", KEY_END);
	KeySet * ks = ksNew (0, KS_END);

	KeySet * conf = ksNew (3, keyNew ("/endpoint", KEY_VALUE, TEST_ENDPOINT, KEY_END),
			       keyNew ("/connectTimeout", KEY_VALUE, TESTCONFIG_CONNECT_TIMEOUT, KEY_END),
			       keyNew ("/subscribeTimeout", KEY_VALUE, TESTCONFIG_SUBSCRIBE_TIMEOUT, KEY_END), KS_END);
	PLUGIN_OPEN ("zeromqsend");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	receiveTimeout = 0;
	receivedKeyName = NULL;
	receivedChangeType = NULL;

	// do not subscribe to Commit messages, this makes the plugin timeout due to no subscribers
	pthread_t * thread = startNotificationReaderThread (NULL);

	plugin->kdbCommit (plugin, ks, parentKey);
	// without timeout we won't return here

	pthread_join (*thread, NULL);

	succeed_if (receiveTimeout, "receiving did not time out");
	succeed_if (receivedKeyName == NULL, "received key name should be unchanged");
	succeed_if (receivedChangeType == NULL, "received change type should be unchanged");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
	elektraFree (receivedKeyName);
	elektraFree (receivedChangeType);
	elektraFree (thread);
}

int main (int argc, char ** argv)
{
	printf ("ZEROMQSEND TESTS\n");
	printf ("================\n\n");

	init (argc, argv);

	int major, minor, patch;
	zmq_version (&major, &minor, &patch);
	printf ("zeromq version is %d.%d.%d\n", major, minor, patch);

	context = zmq_ctx_new ();

	// Test notification from plugin
	test_commit ();

	// test timeouts
	test_timeoutConnect ();
	test_timeoutSubscribe ();

	print_result ("testmod_zeromqsend");

	zmq_ctx_destroy (context);

	return nbError;
}

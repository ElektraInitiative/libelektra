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

#include <kdbioplugin.h> // ElektraIoPluginSetBinding

#include <tests.h>
#include <tests_plugin.h>

#include <pthread.h>

/** change type received by readNotificationFromTestSocket() */
char * receivedChangeType;

/** key name received by readNotificationFromTestSocket() */
char * receivedKeyName;

/** zmq context for tests */
void * context;

/** time in microseconds to wait until zmq connections are established and sending & receiving works */
#define TIME_SETTLE (1000 * 1000)

/** time (100ms in microseconds) before a new socket is created. leaves the system some after binding a socket again */
#define TIME_HOLDOFF (100 * 1000)

/** timeout for tests in seconds */
#define TEST_TIMEOUT 3

/**
 * Create subscriber socket for tests.
 * @internal
 *
 * @param  subscribeFilter filter for subscribtions
 * @return                 new socket
 */
static void * createTestSocket (char * subscribeFilter)
{
	// leave the system some time before binding again
	usleep (TIME_HOLDOFF);

	void * subSocket = zmq_socket (context, ZMQ_SUB);
	int result = zmq_bind (subSocket, "tcp://*:6000");
	if (result != 0)
	{
		yield_error ("zmq_bind failed");
		printf ("zmq error was: %s\n", zmq_strerror (zmq_errno ()));
		exit (-1);
	}
	exit_if_fail (zmq_setsockopt (subSocket, ZMQ_SUBSCRIBE, subscribeFilter, 1) == 0, "subscribe failed"); // subscribe to all messages

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
	void * subSocket = createTestSocket ((char *)filter);

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
		lastErrno = 0;
		int result = zmq_msg_recv (&message, subSocket, ZMQ_DONTWAIT);

		// check for timeout
		if (time (NULL) - start > TEST_TIMEOUT)
		{
			yield_error ("timeout - test failed");
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
			char * buffer = elektraStrNDup (zmq_msg_data (&message), length + 1);
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
pthread_t * startNotificationReaderThread (char * filter)
{
	pthread_t * thread = elektraMalloc (sizeof *thread);
	pthread_create (thread, NULL, notificationReaderThreadMain, filter);
	return thread;
}

static void test_sending (void)
{
	printf ("test sending messages\n");

	char * changeType = "test";
	char * sendingKeyName = "user/foo/bar";

	void * pubSocket = zmq_socket (context, ZMQ_PUB);
	int result = zmq_connect (pubSocket, "tcp://localhost:6000");
	if (result != 0)
	{
		yield_error ("zmq_connect failed!");
		printf ("zmq_connect error: %s\n", zmq_strerror (zmq_errno ()));
		return;
	}

	pthread_t * thread = startNotificationReaderThread ("test");
	usleep (TIME_SETTLE);

	succeed_if (elektraZeroMqSendNotification (pubSocket, changeType, sendingKeyName), "could not send notification failed");

	// Wait for receive thread to finish
	pthread_join (*thread, NULL);

	succeed_if_same_string (changeType, receivedChangeType);
	succeed_if_same_string (sendingKeyName, receivedKeyName);

	zmq_close (pubSocket);

	elektraFree (receivedKeyName);
	elektraFree (receivedChangeType);
	elektraFree (thread);
}

static void test_keyAdded (void)
{
	printf ("test adding keys\n");

	Key * parentKey = keyNew ("system/tests/foo", KEY_END);
	Key * toAdd = keyNew ("system/tests/foo/bar", KEY_END);
	KeySet * ks = ksNew (0, KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("zeromqsend");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// add key to keyset
	ksAppendKey (ks, toAdd);

	pthread_t * thread = startNotificationReaderThread ("Key");
	plugin->kdbSet (plugin, ks, parentKey);
	pthread_join (*thread, NULL);

	succeed_if_same_string ("KeyAdded", receivedChangeType);
	succeed_if_same_string (keyName (toAdd), receivedKeyName);

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
	elektraFree (receivedKeyName);
	elektraFree (receivedChangeType);
	elektraFree (thread);
}

static void test_keyChanged (void)
{
	printf ("test changing keys\n");

	// All keys created by keyNew have the KEY_FLAG_SYNC set and will be
	// detected as changed by the dbus plugin
	// This flag is only cleared after kdbSet or when keys come from a backend.

	Key * parentKey = keyNew ("system/tests/foo", KEY_END);
	Key * toChange = keyNew ("system/tests/foo/bar", KEY_VALUE, "test", KEY_END);
	KeySet * ks = ksNew (1, toChange, KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("zeromqsend");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// change key in keyset
	keySetString (toChange, "new value");

	pthread_t * thread = startNotificationReaderThread ("Key");
	plugin->kdbSet (plugin, ks, parentKey);
	pthread_join (*thread, NULL);

	succeed_if_same_string ("KeyChanged", receivedChangeType);
	succeed_if_same_string (keyName (toChange), receivedKeyName);

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
	elektraFree (receivedKeyName);
	elektraFree (receivedChangeType);
	elektraFree (thread);
}

static void test_keyDeleted (void)
{
	printf ("test deleting keys\n");

	Key * parentKey = keyNew ("system/tests/foo", KEY_END);
	Key * toDelete = keyNew ("system/tests/foo/bar", KEY_END);
	KeySet * ks = ksNew (1, keyDup (toDelete), KS_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("zeromqsend");

	// initial get to save current state
	plugin->kdbGet (plugin, ks, parentKey);

	// remove key from keyset
	Key * deleted = ksLookup (ks, toDelete, KDB_O_POP);
	succeed_if (deleted != NULL, "key was not found");

	pthread_t * thread = startNotificationReaderThread ("Key");
	plugin->kdbSet (plugin, ks, parentKey);
	pthread_join (*thread, NULL);

	succeed_if_same_string ("KeyDeleted", receivedChangeType);
	succeed_if_same_string (keyName (toDelete), receivedKeyName);

	keyDel (toDelete);
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

	// Test basic sending
	test_sending ();

	// Test added, changed & deleted
	test_keyAdded ();
	test_keyChanged ();
	test_keyDeleted ();

	printf ("\ntestmod_zeromqsend RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	zmq_ctx_destroy (context);

	return nbError;
}

/**
 * @file
 *
 * @brief message hub for ZeroMq
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 * This programm allows multiple subscribers and multiple publishers by
 * binding XPUB & XSUB sockets at known endpoints.
 */
#include <signal.h> // signal
#include <stdio.h>  // printf

#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <elektra/kdb/kdb.h>
#include <zmq.h> // ZeroMq function

void * context;
void * xSubSocket;
void * xPubSocket;

static void onSignal (int signal)
{
	if (signal == SIGINT)
	{
		printf ("Stopping ZeroMq message hub...");

		zmq_close (xSubSocket);
		zmq_close (xPubSocket);
		zmq_ctx_destroy (context);

		printf ("done\n");
	}
}

int main (void)
{
	printf ("\nlightweight zeromq message hub\n");

	// exit on SIGINT
	signal (SIGINT, onSignal);

	KeySet * config = ksNew (2, KS_END);

	Key * parentKey = keyNew ("/sw/elektra/hub-zeromq/#0/current", KEY_END);
	Key * configXSubEndpoint = keyDup (parentKey, KEY_CP_ALL);
	keyAddBaseName (configXSubEndpoint, "bind_xsub");
	Key * configXPubEndpoint = keyDup (parentKey, KEY_CP_ALL);
	keyAddBaseName (configXPubEndpoint, "bind_xpub");
	KDB * kdb = kdbOpen (NULL, parentKey);
	if (kdb == NULL)
	{
		printf ("could not open KDB. aborting\n");
		return -1;
	}

	const char * xSubEndpoint = "tcp://127.0.0.1:6000";
	const char * xPubEndpoint = "tcp://127.0.0.1:6001";
	kdbGet (kdb, config, parentKey);
	Key * xSubEndpointKey = ksLookup (config, configXSubEndpoint, 0);
	if (xSubEndpointKey)
	{
		xSubEndpoint = keyString (xSubEndpointKey);
	}
	Key * xPubEndpointKey = ksLookup (config, configXPubEndpoint, 0);
	if (xPubEndpointKey)
	{
		xPubEndpoint = keyString (xPubEndpointKey);
	}

	keyDel (configXSubEndpoint);
	keyDel (configXPubEndpoint);
	kdbClose (kdb, parentKey);
	keyDel (parentKey);

	context = zmq_ctx_new ();
	xSubSocket = zmq_socket (context, ZMQ_XSUB);
	xPubSocket = zmq_socket (context, ZMQ_XPUB);
	int result;
	result = zmq_bind (xSubSocket, xSubEndpoint);
	if (result != 0)
	{
		printf ("could not bind XSUB on %s socket: %s\n", xSubEndpoint, zmq_strerror (zmq_errno ()));
		zmq_close (xSubSocket);
		zmq_close (xPubSocket);
		zmq_ctx_destroy (context);
		return -1;
	}
	result = zmq_bind (xPubSocket, xPubEndpoint);
	if (result != 0)
	{
		printf ("could not bind XPUB on %s socket: %s\n", xPubEndpoint, zmq_strerror (zmq_errno ()));
		zmq_close (xSubSocket);
		zmq_close (xPubSocket);
		zmq_ctx_destroy (context);
		return -1;
	}

	printf ("listening on %s (XSUB for zeromqsend)\n", xSubEndpoint);
	printf ("listening on %s (XPUB for zeromqrecv)\n", xPubEndpoint);
	printf ("hub is running\n");
	ksDel (config);

	// forward messages between sockets
	// will return on zmq_ctx_destroy()
	zmq_proxy (xPubSocket, xSubSocket, NULL);

	return 0;
}

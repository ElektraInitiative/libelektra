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

#include <kdb.h> // KDB

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

	ElektraKeyset * config = elektraKeysetNew (2, ELEKTRA_KS_END);

	ElektraKey * parentKey = elektraKeyNew ("/sw/elektra/hub-zeromq/#0/current", ELEKTRA_KEY_END);
	ElektraKey * configXSubEndpoint = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (configXSubEndpoint, "bind_xsub");
	ElektraKey * configXPubEndpoint = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (configXPubEndpoint, "bind_xpub");
	ElektraKdb * kdb = elektraKdbOpen (NULL, parentKey);
	if (kdb == NULL)
	{
		printf ("could not open KDB. aborting\n");
		return -1;
	}

	const char * xSubEndpoint = "tcp://127.0.0.1:6000";
	const char * xPubEndpoint = "tcp://127.0.0.1:6001";
	elektraKdbGet (kdb, config, parentKey);
	ElektraKey * xSubEndpointKey = elektraKeysetLookup (config, configXSubEndpoint, 0);
	if (xSubEndpointKey)
	{
		xSubEndpoint = elektraKeyString (xSubEndpointKey);
	}
	ElektraKey * xPubEndpointKey = elektraKeysetLookup (config, configXPubEndpoint, 0);
	if (xPubEndpointKey)
	{
		xPubEndpoint = elektraKeyString (xPubEndpointKey);
	}

	elektraKeyDel (configXSubEndpoint);
	elektraKeyDel (configXPubEndpoint);
	elektraKdbClose (kdb, parentKey);
	elektraKeyDel (parentKey);

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
	elektraKeysetDel (config);

	// forward messages between sockets
	// will return on zmq_ctx_destroy()
	zmq_proxy (xPubSocket, xSubSocket, NULL);

	return 0;
}

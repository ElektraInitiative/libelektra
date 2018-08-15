/**
 * @file
 *
 * @brief Compilation test for ZeroMQ.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#include <zmq.h>

int main (void)
{
	void * context = zmq_ctx_new ();
	void * test = zmq_socket (context, ZMQ_SUB);
	zmq_close (test);
	zmq_ctx_destroy (context);
	return 0;
}

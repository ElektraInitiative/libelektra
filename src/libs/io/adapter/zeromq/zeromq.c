/**
 * @file
 *
 * @brief I/O Adapter for D-Bus.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#include <elektra/kdbhelper.h>
#include <kdbassert.h>
#include <kdbio/adapters/zeromq.h>
#include <kdblogger.h>

#include <stdlib.h>
#include <string.h>

typedef struct _ElektraIoAdapterZeroMqHandle
{
	ElektraIoInterface * ioBinding;
	ElektraIoIdleOperation * dispatchIdle;
	ElektraIoFdOperation * fdOp;

	void * socket;

	ElektraIoAdapterZeroMqCallbackType type;
	ElektraIoAdapterZeroMqCallback callback;
	void * callbackContext;

} _ElektraIoAdapterZeroMqHandle;

static void zmqAdapterDispatch (_ElektraIoAdapterZeroMqHandle * handle)
{
	ElektraIoIdleOperation * dispatchIdle = handle->dispatchIdle;

	// Check if socket is readable or writable
	uint events = 0;
	size_t events_len = sizeof (events);
	zmq_getsockopt (handle->socket, ZMQ_EVENTS, &events, &events_len);

	if ((events & ZMQ_POLLIN) && handle->type == ELEKTRA_IO_ADAPTER_ZEROMQCB_READ)
	{
		handle->callback (handle->socket, handle->callbackContext);
	}
	if ((events & ZMQ_POLLOUT) && handle->type == ELEKTRA_IO_ADAPTER_ZEROMQCB_WRITE)
	{
		handle->callback (handle->socket, handle->callbackContext);
	}

	if (!(events & ZMQ_POLLIN) && !(events & ZMQ_POLLOUT))
	{
		// Disable idle, nothing to do anymore
		elektraIoIdleSetEnabled (dispatchIdle, 0);
		elektraIoBindingUpdateIdle (dispatchIdle);
	}
}

static void zmqAdapterIdleCallback (ElektraIoIdleOperation * idleOp)
{
	_ElektraIoAdapterZeroMqHandle * handle = elektraIoIdleGetData (idleOp);
	ELEKTRA_NOT_NULL (handle);
	zmqAdapterDispatch (handle);
}

static void zmqAdapterFdCallback (ElektraIoFdOperation * fdOp, int flags)
{
	_ElektraIoAdapterZeroMqHandle * handle = elektraIoFdGetData (fdOp);
	ELEKTRA_NOT_NULL (handle);
	ElektraIoIdleOperation * dispatchIdle = handle->dispatchIdle;

	if (flags & ELEKTRA_IO_READABLE || flags & ELEKTRA_IO_WRITABLE)
	{
		elektraIoIdleSetEnabled (dispatchIdle, 1);
		elektraIoBindingUpdateIdle (dispatchIdle);
	}

	// Dispatch available data
	zmqAdapterDispatch (handle);
}

ElektraIoAdapterZeroMqHandle * elektraIoAdapterZeroMqAttach (void * socket, ElektraIoInterface * ioBinding,
							     ElektraIoAdapterZeroMqCallbackType type,
							     ElektraIoAdapterZeroMqCallback callback, void * context)
{
	if (socket == NULL)
	{
		ELEKTRA_LOG_WARNING ("socket cannot be null");
		return NULL;
	}
	if (ioBinding == NULL)
	{
		ELEKTRA_LOG_WARNING ("ioBinding cannot be null");
		return NULL;
	}
	if (callback == NULL)
	{
		ELEKTRA_LOG_WARNING ("callback cannot be null");
		return NULL;
	}

	_ElektraIoAdapterZeroMqHandle * handle = elektraMalloc (sizeof (*handle));
	if (!handle)
	{
		return NULL;
	}
	handle->ioBinding = ioBinding;
	handle->socket = socket;
	handle->type = type;
	handle->callback = callback;
	handle->callbackContext = context;

	int fdOpFlags;
	switch (type)
	{
	case ELEKTRA_IO_ADAPTER_ZEROMQCB_READ:
		fdOpFlags = ELEKTRA_IO_READABLE;
		break;
	case ELEKTRA_IO_ADAPTER_ZEROMQCB_WRITE:
		fdOpFlags = ELEKTRA_IO_WRITABLE;
		break;
	default:
		ELEKTRA_LOG_WARNING ("invalid callback type: %d", type);
		return NULL;
		break;
	}

	// Add sockets file descriptor to I/O binding
	int fd;
	size_t fd_len = sizeof (fd);
	if (zmq_getsockopt (socket, ZMQ_FD, &fd, &fd_len) != 0)
	{
		ELEKTRA_LOG_WARNING ("zmq_getsockopt failed: could not get file descriptor from socket");
		return NULL;
	}
	ElektraIoFdOperation * fdOp = elektraIoNewFdOperation (fd, fdOpFlags, 1, zmqAdapterFdCallback, handle);
	if (fdOp == NULL)
	{
		elektraFree (handle);
		return NULL;
	}
	handle->fdOp = fdOp;
	if (!elektraIoBindingAddFd (ioBinding, fdOp))
	{
		elektraFree (fdOp);
		elektraFree (handle);
		return NULL;
	}

	// Add timeout for reading messages
	ElektraIoIdleOperation * dispatchIdle = elektraIoNewIdleOperation (0, zmqAdapterIdleCallback, handle);
	if (!dispatchIdle)
	{
		elektraIoBindingRemoveFd (fdOp);
		elektraFree (fdOp);
		elektraFree (handle);
		return NULL;
	}
	handle->dispatchIdle = dispatchIdle;
	if (!elektraIoBindingAddIdle (ioBinding, dispatchIdle))
	{
		elektraFree (dispatchIdle);
		elektraIoBindingRemoveFd (fdOp);
		elektraFree (fdOp);
		elektraFree (handle);
		return NULL;
	}

	return handle;
}

void elektraIoAdapterZeroMqSetContext (ElektraIoAdapterZeroMqHandle * handle, void * context)
{
	handle->callbackContext = context;
}

int elektraIoAdapterZeroMqDetach (ElektraIoAdapterZeroMqHandle * handle)
{

	if (!elektraIoBindingRemoveIdle (handle->dispatchIdle))
	{
		ELEKTRA_LOG_WARNING ("could not remove idle operation");
	}
	if (!elektraIoBindingRemoveFd (handle->fdOp))
	{
		ELEKTRA_LOG_WARNING ("could not remove fd operation");
	}
	elektraFree (handle->dispatchIdle);
	elektraFree (handle->fdOp);
	elektraFree (handle);

	return 1;
}

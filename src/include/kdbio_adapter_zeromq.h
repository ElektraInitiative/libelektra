/**
 * @file
 *
 * @brief I/O Adapter for D-Bus.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */
#ifndef __ELEKTAR_IO_ADAPTER_ZEROMQ_H__
#define __ELEKTAR_IO_ADAPTER_ZEROMQ_H__

//#include <zmq.h>
#include <stdlib.h>
#include <string.h>

#include <kdbio.h>


/** callback types  */
typedef enum {

	/** callback is called when socket is readable */
	ELEKTRA_IO_ADAPTER_ZEROMQCB_READ = 1 << 0,

	/** callback is called when socket is writable */
	ELEKTRA_IO_ADAPTER_ZEROMQCB_WRITE = 1 << 1,

} ElektraIoAdapterZeroMqCallbackType;

/**
 * Callback for ZeroMq adapter callbacks.
 *
 * @param  socket   zeromq socket
 * @param  context  callback context supplied to elektraIoZeroMqAdapterAttach()
 */
typedef void (*ElektraIoAdapterZeroMqCallback) (void * socket, void * context);

/**
 * D-Bus Adapter Handle.
 *
 * Returned by elektraIoAdapterDbusAttach().
 */
typedef struct _ElektraIoAdapterZeroMqHandle ElektraIoAdapterZeroMqHandle;

/**
 * Attach to ZeroMq socket.
 *
 * The callback is called whenever socket is readable or writable (depending on
 * type) and data can be processed.
 * The callback should not do blocking calls (e.g. use ZMQ_DONTWAIT for zmq_*recv()).
 * Since ZeroMq guarantees that multipart messages arrive at once, data will be
 * available.
 * New adapter instances are enabled.
 *
 * @param  socket    ZeroMq socket
 * @param  ioBinding I/O binding
 * @param  type      callback type (@see ElektraIoAdapterZeroMqCallbackType)
 * @param  callback  callback
 * @param  context   callback context (gets passed to callback)
 * @return           handle to use with elektraIoZeroMqAdapterDetach() or NULL on error
 */
ElektraIoAdapterZeroMqHandle * elektraIoAdapterZeroMqAttach (void * socket, ElektraIoInterface * ioBinding,
							     ElektraIoAdapterZeroMqCallbackType type,
							     ElektraIoAdapterZeroMqCallback callback, void * context);

/**
 * Enable or disable ZeroMq adapter.
 *
 * Allows to disable polling etc. so that the callback is no called anymore
 * until the adapter handle is re-enabled.
 *
 * @param handle  adapter handle
 * @param enabled 1 to enable, any other value to disable
 */
int elektraIoZeroMqAdapterSetEnabled (ElektraIoAdapterZeroMqHandle * handle, int enabled);

/**
 * Set the callback context for a ZeroMq adapter handle.
 *
 * The previous context is replaced and not freed.
 *
 * @param handle  adapter handle
 * @param context new callback context
 */
void elektraIoZeroMqAdapterSetContext (ElektraIoAdapterZeroMqHandle * handle, void * context);

/**
 * Remove ZeroMq socket from I/O binding.
 *
 * This function frees the passed handle.
 *
 * @param  handle adapter handle
 * @retval 1 on success
 * @retval 0 on error
 */
int elektraIoZeroMqAdapterDetach (ElektraIoAdapterZeroMqHandle * handle);

#endif

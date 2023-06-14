/**
 * @file
 *
 * @brief I/O UV binding.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#include <stdlib.h>

#include <uv.h>

#include <elektra/io/api.h>
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>
#include <internal/utility/assert.h>
#include <internal/utility/logger.h>

/**
 * Container for required additional information for
 * I/O binding operations & libuv Handles
 */
typedef struct UvBindingData
{
	union
	{
		ElektraIoFdOperation * fd;
		ElektraIoTimerOperation * timer;
		ElektraIoIdleOperation * idle;
	} operation;
	union
	{
		uv_poll_t fd;
		uv_timer_t timer;
		uv_idle_t idle;
	} handle;
} UvBindingData;

/**
 * Convert I/O flags to libuv event bit mask
 * @param  flags I/O flags bit mask
 * @return       libuv events bit mask
 */
static int flagsToEvents (int flags)
{
	int events = 0;
	if (flags & ELEKTRA_IO_READABLE)
	{
		events |= UV_READABLE;
	}
	if (flags & ELEKTRA_IO_WRITABLE)
	{
		events |= UV_WRITABLE;
	}
	return events;
}

/**
 * Convert libuv event bit mask to I/O flags
 * @param  events libuv events bit mask
 * @return        I/O flags bit mask
 */
static int eventsToFlags (int events)
{
	int flags = 0;
	if (events & UV_READABLE)
	{
		flags |= ELEKTRA_IO_READABLE;
	}
	if (events & UV_WRITABLE)
	{
		flags |= ELEKTRA_IO_WRITABLE;
	}
	return flags;
}

/**
 * @internal
 * Create new data structure for binding operations.
 *
 * @return  new data structure
 */
static UvBindingData * newBindingData (void)
{
	UvBindingData * bindingData = elektraCalloc (sizeof (*bindingData));
	if (bindingData == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraCalloc failed");
		return NULL;
	}

	return bindingData;
}

/**
 * @internal
 * Get error message from libuv.
 *
 * Function is only used if logging is enabled.
 *
 * @param  result return von libuv operation
 * @param  loop   loop
 * @return        error message
 */
static ELEKTRA_UNUSED const char * ioUvBindingGetError (int result ELEKTRA_UNUSED, uv_loop_t * loop ELEKTRA_UNUSED)
{
#ifdef HAVE_LIBUV0
	return uv_strerror (uv_last_error (loop));
#else
	return uv_strerror (result);
#endif
}

/**
 * Free allocated memory after a handle has been closed.
 * Called by libuv.
 *
 * @param handle any libuv handle type (e.g. poll, timer, idle)
 */
static void ioUvBindingHandleClosedCallback (uv_handle_t * handle)
{
	ELEKTRA_NOT_NULL (handle->data);
	UvBindingData * bindingData = (UvBindingData *) handle->data;

	elektraFree (bindingData);
}

/**
 * Converts the events bit mask and calls the associated callback
 * Called by libuv whenever a file descriptor status has changed.
 *
 * @param handle libuv poll handle
 * @param status 0 means no error
 * @param events events bit mask
 */
static void ioUvBindingFdCallback (uv_poll_t * handle, int status, int events)
{
	if (status != 0)
	{
		return;
	}

	ELEKTRA_NOT_NULL (handle->data);
	UvBindingData * bindingData = (UvBindingData *) handle->data;
	ElektraIoFdOperation * fdOp = (ElektraIoFdOperation *) bindingData->operation.fd;

	elektraIoFdGetCallback (fdOp) (fdOp, eventsToFlags (events));
}

/**
 * Calls the associated callback.
 * Called by libuv whenever a timer has elapsed.
 *
 * @param handle libuv timer handle
 */
#ifdef HAVE_LIBUV0
static void ioUvBindingTimerCallback (uv_timer_t * handle, int unknown ELEKTRA_UNUSED)
#else
static void ioUvBindingTimerCallback (uv_timer_t * handle)
#endif
{
	ELEKTRA_NOT_NULL (handle->data);
	UvBindingData * bindingData = (UvBindingData *) handle->data;
	ElektraIoTimerOperation * timerOp = (ElektraIoTimerOperation *) bindingData->operation.timer;

	elektraIoTimerGetCallback (timerOp) (timerOp);
}

/**
 * Calls the associated callback.
 * Called by libuv whenever an idle operation can perform its operations.
 *
 * @param handle libuv idle handle
 */
#ifdef HAVE_LIBUV0
static void ioUvBindingIdleCallback (uv_idle_t * handle, int unknown ELEKTRA_UNUSED)
#else
static void ioUvBindingIdleCallback (uv_idle_t * handle)
#endif
{
	ELEKTRA_NOT_NULL (handle->data);
	UvBindingData * bindingData = (UvBindingData *) handle->data;
	ElektraIoIdleOperation * idleOp = (ElektraIoIdleOperation *) bindingData->operation.idle;

	elektraIoIdleGetCallback (idleOp) (idleOp);
}

/**
 * Update information about a file descriptor watched by I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateFd
 */
static int ioUvBindingUpdateFd (ElektraIoFdOperation * fdOp)
{
	ELEKTRA_NOT_NULL (elektraIoFdGetBindingData (fdOp));
	UvBindingData * bindingData = (UvBindingData *) elektraIoFdGetBindingData (fdOp);

	int result;
	if (elektraIoFdIsEnabled (fdOp))
	{
		result = uv_poll_start (&bindingData->handle.fd, flagsToEvents (elektraIoFdGetFlags (fdOp)), ioUvBindingFdCallback);
	}
	else
	{
		result = uv_poll_stop (&bindingData->handle.fd);
	}

	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could not update poll: %s", ioUvBindingGetError (result, bindingData->handle.fd.loop));
		return 0;
	}
	else
	{
		return 1;
	}
}

/**
 * Add file descriptor to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddFd
 */
static int ioUvBindingAddFd (ElektraIoInterface * binding, ElektraIoFdOperation * fdOp)
{
	UvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	uv_loop_t * loop = (uv_loop_t *) elektraIoBindingGetData (binding);
	ELEKTRA_NOT_NULL (loop);

	elektraIoFdSetBindingData (fdOp, bindingData);
	bindingData->operation.fd = fdOp;
	bindingData->handle.fd.data = bindingData;

	int result = uv_poll_init (loop, &bindingData->handle.fd, elektraIoFdGetFd (fdOp));
	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could not initialize poll: %s", ioUvBindingGetError (result, loop));
		return 0;
	}

	// Start polling if enabled
	if (elektraIoFdIsEnabled (fdOp))
	{
		ioUvBindingUpdateFd (fdOp);
	}

	return 1;
}

/**
 * Remove file descriptor from I/O binding.
 * @see kdbio.h ::ElektraIoBindingRemoveFd
 */
static int ioUvBindingRemoveFd (ElektraIoFdOperation * fdOp)
{
	ELEKTRA_NOT_NULL (elektraIoFdGetBindingData (fdOp));
	UvBindingData * bindingData = (UvBindingData *) elektraIoFdGetBindingData (fdOp);
	int result = uv_poll_stop (&bindingData->handle.fd);
	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could not stop polling: %s", ioUvBindingGetError (result, bindingData->handle.fd.loop));
	}
	uv_close ((uv_handle_t *) &bindingData->handle.fd, ioUvBindingHandleClosedCallback);
	return 1;
}

/**
 * Update timer in I/O binding.
 * @see kdbio.h ::ElektraIoBindingUpdateTimer
 */
static int ioUvBindingUpdateTimer (ElektraIoTimerOperation * timerOp)
{
	ELEKTRA_NOT_NULL (elektraIoTimerGetBindingData (timerOp));
	UvBindingData * bindingData = (UvBindingData *) elektraIoTimerGetBindingData (timerOp);
	int result;
	if (elektraIoTimerIsEnabled (timerOp))
	{
		unsigned int interval = elektraIoTimerGetInterval (timerOp);
		result = uv_timer_start (&bindingData->handle.timer, ioUvBindingTimerCallback, interval, interval);
	}
	else
	{
		result = uv_timer_stop (&bindingData->handle.timer);
	}

	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could update timer: %s", ioUvBindingGetError (result, bindingData->handle.timer.loop));
		return 0;
	}
	else
	{
		return 1;
	}
}

/**
 * Add timer for I/O binding.
 * @see kdbio.h ::ElektraIoBindingAddTimer
 */
static int ioUvBindingAddTimer (ElektraIoInterface * binding, ElektraIoTimerOperation * timerOp)
{
	UvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	uv_loop_t * loop = (uv_loop_t *) elektraIoBindingGetData (binding);
	ELEKTRA_NOT_NULL (loop);

	elektraIoTimerSetBindingData (timerOp, bindingData);
	bindingData->operation.timer = timerOp;
	bindingData->handle.timer.data = bindingData;

	int result;
	result = uv_timer_init (loop, &bindingData->handle.timer);
	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could not initialize timer: %s", ioUvBindingGetError (result, loop));
		return 0;
	}

	// Start timer if enabled
	if (elektraIoTimerIsEnabled (timerOp))
	{
		ioUvBindingUpdateTimer (timerOp);
	}

	return 1;
}

/**
 * Remove timer from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveTimer
 */
static int ioUvBindingRemoveTimer (ElektraIoTimerOperation * timerOp)
{
	ELEKTRA_NOT_NULL (elektraIoTimerGetBindingData (timerOp));
	UvBindingData * bindingData = (UvBindingData *) elektraIoTimerGetBindingData (timerOp);
	int result = uv_timer_stop (&bindingData->handle.timer);
	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could not stop timer: %s", ioUvBindingGetError (result, bindingData->handle.timer.loop));
	}
	uv_close ((uv_handle_t *) &bindingData->handle.timer, ioUvBindingHandleClosedCallback);
	return 1;
}

/**
 * Update idle operation in I/O binding
 * @see kdbio.h ::ElektraIoBindingUpdateIdle
 */
static int ioUvBindingUpdateIdle (ElektraIoIdleOperation * idleOp)
{
	ELEKTRA_NOT_NULL (elektraIoIdleGetBindingData (idleOp));
	UvBindingData * bindingData = (UvBindingData *) elektraIoIdleGetBindingData (idleOp);
	int result;
	if (elektraIoIdleIsEnabled (idleOp))
	{
		result = uv_idle_start (&bindingData->handle.idle, ioUvBindingIdleCallback);
	}
	else
	{
		result = uv_idle_stop (&bindingData->handle.idle);
	}

	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could update idle: %s", ioUvBindingGetError (result, bindingData->handle.idle.loop));
		return 0;
	}
	else
	{
		return 1;
	}
}

/**
 * Add idle operation to I/O binding
 * @see kdbio.h ::ElektraIoBindingAddIdle
 */
static int ioUvBindingAddIdle (ElektraIoInterface * binding, ElektraIoIdleOperation * idleOp)
{
	UvBindingData * bindingData = newBindingData ();
	if (bindingData == NULL)
	{
		return 0;
	}
	uv_loop_t * loop = (uv_loop_t *) elektraIoBindingGetData (binding);
	ELEKTRA_NOT_NULL (loop);

	elektraIoIdleSetBindingData (idleOp, bindingData);
	bindingData->operation.idle = idleOp;
	bindingData->handle.idle.data = bindingData;

	int result;
	result = uv_idle_init (loop, &bindingData->handle.idle);
	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could not initialize idle: %s", ioUvBindingGetError (result, loop));
		return 0;
	}

	// Add idle to loop if enabled
	if (elektraIoIdleIsEnabled (idleOp))
	{
		ioUvBindingUpdateIdle (idleOp);
	}

	return 1;
}

/**
 * Remove idle operation from I/O binding
 * @see kdbio.h ::ElektraIoBindingRemoveIdle
 */
static int ioUvBindingRemoveIdle (ElektraIoIdleOperation * idleOp)
{
	ELEKTRA_NOT_NULL (elektraIoIdleGetBindingData (idleOp));
	UvBindingData * bindingData = (UvBindingData *) elektraIoIdleGetBindingData (idleOp);
	int result = uv_idle_stop (&bindingData->handle.idle);
	if (result != 0)
	{
		ELEKTRA_LOG_WARNING ("could not stop idle: %s", ioUvBindingGetError (result, bindingData->handle.idle.loop));
	}
	uv_close ((uv_handle_t *) &bindingData->handle.idle, ioUvBindingHandleClosedCallback);
	return 1;
}

/**
 * Cleanup
 * @param  binding I/O binding
 * @see kdbio.h ::ElektraIoBindingCleanup
 */
static int ioUvBindingCleanup (ElektraIoInterface * binding)
{
	ELEKTRA_NOT_NULL (binding);
	elektraFree (binding);
	return 1;
}

/**
 * Create and initialize a new I/O binding.
 * @param  loop Loop to use for I/O operations
 * @return      Populated I/O interface
 */
ElektraIoInterface * elektraIoUvNew (uv_loop_t * loop)
{
	if (loop == NULL)
	{
		ELEKTRA_LOG_WARNING ("loop was NULL");
		return NULL;
	}
	// Initialize I/O interface
	ElektraIoInterface * binding = elektraIoNewBinding (
		// file descriptors
		ioUvBindingAddFd, ioUvBindingUpdateFd, ioUvBindingRemoveFd,
		// timers
		ioUvBindingAddTimer, ioUvBindingUpdateTimer, ioUvBindingRemoveTimer,
		// idle
		ioUvBindingAddIdle, ioUvBindingUpdateIdle, ioUvBindingRemoveIdle,
		// cleanup
		ioUvBindingCleanup);
	if (binding == NULL)
	{
		ELEKTRA_LOG_WARNING ("elektraIoNewBinding failed");
		return NULL;
	}

	// Save the libuv loop we are using
	elektraIoBindingSetData (binding, loop);

	return binding;
}
